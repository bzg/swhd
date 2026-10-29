(ns swhd.core
  "Local daemon behind swh.el: computes SWHIDs (swhid-clj), checks them
  against the Software Heritage archive (swh-api), caches positive
  answers, and serves a small JSON API over HTTP on 127.0.0.1.

  GET  /ping             liveness check, with the daemon's version and
                         whether it has a token of its own
  GET  /tree?path=DIR    every artifact of the directory: swhid, dir?,
                         archived (Merkle-pruned: an archived directory
                         covers everything under it), and provenance
                         when the provenance cache holds it
  GET  /file?path=FILE   one artifact: swhid, archived (a directory
                         path yields its dir SWHID); in a Git repo, an
                         archived ancestor directory covers it
  GET  /swhid?path=FILE[&lines=A[-B]][&archived]
                         the artifact as a qualified SWHID (origin,
                         visit, anchor, path, lines), verified against
                         the archive, with its browse URL; a 400 for a
                         directory where nothing is tracked; with
                         archived, the version the archive holds,
                         without lines (the working tree's, with them,
                         when it holds none; a directory where nothing
                         is tracked is a 400 only then)
  GET  /origin?path=DIR[&upstream]
                         the repo's origin remote in the archive:
                         archived, date of the last archival; with
                         upstream, for an archived origin, also the
                         current branch's upstream as a revision SWHID
                         and whether the archive holds it
  GET  /visits?path=DIR[&n=10]
                         the last n archival visits of the origin
  POST /save?path=DIR    save code now on the repo's origin remote
  GET  /provenance?swhid=SWHID
                         where the archive found the artifact: the
                         qualified SWHID (origin, anchor) of its
                         provenance API, null when none; needs a token
                         with the provenance permission

  Environment: SWHD_PORT (default 8642), SWH_API_TOKEN (a client's own
  Authorization: Bearer header takes precedence)."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [org.httpkit.server :as srv]
            [swh.api.v1 :as api]
            [swhd.cache :as cache]
            [swhd.origin :as origin]
            [swhd.scan :as scan]
            [swhid.core :as swhid]
            [swhid.qualifier :as q]))

;;; HTTP plumbing

(def version
  "This daemon's version, which /ping gives: clients such as swh.el use
  it to tell an outdated daemon. Bump it with each release tag."
  "0.7.0")

(defn- fail
  "Throw a daemon error answered with HTTP `status`."
  [status msg & [data]]
  (throw (ex-info msg (assoc data :swhd/status status))))

(defn query-params
  "The query string as a map, nil when it cannot be decoded. A \"+\" is
  a plus sign, not a space: clients encode spaces as %20, and a path
  such as /tmp/c++ must reach us as is."
  [query-string]
  (try
    (into {}
          (for [kv (str/split (or query-string "") #"&")
                :when (seq kv)
                :let [[k v] (str/split kv #"=" 2)]]
            [k (java.net.URLDecoder/decode (str/replace (or v "") "+" "%2B") "UTF-8")]))
    (catch IllegalArgumentException _ nil)))

(defn bearer-token
  "The token of the request's Authorization: Bearer header, if any."
  [headers]
  (some->> (get headers "authorization")
           (re-find #"(?i)^bearer\s+(\S+)$")
           second))

(defn- respond
  "A response whose `body` (a map) the handler serializes to JSON."
  [status body]
  {:status status :body body})

(defn- unreachable?
  "True when `e`, or one of its causes, is a network failure. Other
  IOExceptions (git not on the PATH, an unreadable file) are not: they
  have nothing to do with the archive."
  [e]
  (some #(or (instance? java.net.SocketException %)
             (instance? java.net.UnknownHostException %)
             (instance? java.net.http.HttpTimeoutException %))
        (take-while some? (iterate ex-cause e))))

(defn- error-response
  "JSON error for an exception. Daemon errors carry their HTTP status
  under :swhd/status; an error answered by the archive (swh-api puts
  its status under :status) is a 502 with that status as
  :upstream-status and the reason it gives, if any, appended to the
  error message (\"Bearer token expired...\"); a rate limit a 429, an
  unreachable archive (see `unreachable?`) a 503; anything else a 500."
  [e]
  (let [{:keys [rate-limited retry-after status body] swhd-status :swhd/status}
        (ex-data e)
        ;; ex-message is nil for a bare NPE and the like
        msg (or (ex-message e) (str (class e)))]
    (cond
      rate-limited (respond 429 {:error "SWH API rate limit" :retry-after retry-after})
      swhd-status (respond swhd-status {:error msg})
      status (respond 502 {:error (if-let [reason (when (map? body) (get body "reason"))]
                                    (str msg ": " reason)
                                    msg)
                           :upstream-status status})
      (unreachable? e)
      (respond 503 {:error "Cannot reach the Software Heritage archive"})
      :else (respond 500 {:error msg}))))

(defn- local-request?
  "True unless the request comes from a browser page: a cross-origin
  request carries an Origin header, and DNS rebinding a foreign Host.
  Requests without a Host header (not from a browser) are accepted."
  [headers]
  (let [host (some-> (get headers "host") (str/replace #":\d+$" ""))]
    (and (nil? (get headers "origin"))
         (or (nil? host) (contains? #{"127.0.0.1" "localhost"} host)))))

(defn log-stderr
  "Default logger: one line on stderr."
  [msg]
  (binding [*out* *err*]
    (println (str (.truncatedTo (java.time.Instant/now) java.time.temporal.ChronoUnit/SECONDS)
                  " " msg))))

;;; Archive access, through the caches
;;
;; A context map `ctx` holds what every route needs:
;;   :cache       swhd.cache, archived SWHIDs (persistent)
;;   :api-opts    swh.api.v1 options (token, base-url, :request hook)
;;   :known-fn    api/known caching its positive answers, see `known-fn`
;;   :origin-opts swhd.origin/lookup options (visits cache, ttl, log)

(defn- known-fn
  "api/known on `api-opts`, caching every positive answer as soon as it
  arrives: those of each call that succeeds, and those of the batches
  that succeeded within a call that fails (swh-api puts them under
  :partial in the metadata of the ex-data). A 429 halfway through a
  large scan is thus not paid twice, whichever round it hits."
  [cache api-opts log]
  (fn [swhids]
    (log (format "known: asking the archive about %d SWHIDs" (count swhids)))
    (try
      (let [result (api/known swhids api-opts)]
        (cache/add! cache (scan/positives result))
        result)
      (catch clojure.lang.ExceptionInfo e
        (cache/add! cache (scan/positives (:partial (meta (ex-data e)))))
        (throw e)))))

(defn- lookup!
  "swhd.scan/status of the `artifact` against the cache, which also
  learns what an archived ancestor covers (the API's answers are already
  in it, see `known-fn`)."
  [{:keys [cache known-fn]} artifact]
  (let [{:keys [newly-known] :as result}
        (scan/status artifact known-fn #(cache/known? cache %))]
    (cache/add! cache newly-known)
    result))

(defn- core-swhid!
  "The core SWHID string of `s`, a 400 when it is not a SWHID."
  [s]
  (when-not s (fail 400 "Missing swhid parameter"))
  (try (swhid/swhid->string (swhid/core-swhid (swhid/parse s)))
       (catch clojure.lang.ExceptionInfo _
         (fail 400 "Invalid SWHID" {:swhid s}))))

(defn- provenance!
  "Where the archive found the core SWHID string `s`, as
  swh.api.v1/whereis says: from the provenance cache, which keeps what
  is found for good, else from the archive. That it knows none is
  remembered in `misses` for `ttl-ms`: the archive may get it."
  [{:keys [provenance misses ttl-ms api-opts log]} s]
  (or (cache/provenance provenance s)
      (let [now (System/currentTimeMillis)]
        (when-not (origin/fresh (get @misses s) now ttl-ms)
          (log (str "provenance: asking the archive about " s))
          (if-let [found (api/whereis s api-opts)]
            (do (cache/add-provenance! provenance s found)
                (swap! misses dissoc s)
                found)
            (do (swap! misses assoc s {:value true :at now})
                nil))))))

(defn- with-provenances
  "The `tree` of swhd.scan/scan, each node the provenance cache holds a
  provenance for carrying it: what is known already, never asked."
  [{:keys [provenance]} tree]
  (into (sorted-map)
        (map (fn [[p {:keys [swhid] :as node}]]
               [p (if-let [found (cache/provenance provenance swhid)]
                    (assoc node :provenance found)
                    node)]))
        tree))

(defn- resolve-origin!
  "swhd.origin/lookup for the repository holding `path`, with the
  repository's root under :repo; a 400 outside a Git repository or
  without a remote."
  [{:keys [api-opts origin-opts]} path]
  (let [repo (or (scan/repo-root (scan/normalize path))
                 (fail 400 "Not a Git repository" {:path path}))]
    (assoc (or (origin/lookup repo api-opts origin-opts)
               (fail 400 "No remote" {:path path}))
           :repo repo)))

(defn- archived?
  "True when the archive holds the core SWHID string `s`: the cache
  first, then the archive."
  [{:keys [cache known-fn]} s]
  (boolean (or (cache/known? cache s) (get (known-fn [s]) s))))

(defn- revision
  "The commit hash `commit` as a core revision SWHID string."
  [commit]
  (swhid/swhid->string (swhid/make :revision commit)))

(defn- upstream-revision
  "The upstream of `repo`'s current branch as a core revision SWHID
  string, see swhd.origin/upstream-commit."
  [repo]
  (some-> (origin/upstream-commit repo) revision))

;;; Qualified SWHIDs

(defn- parse-lines
  "\"9-15\" -> [9 15], \"9\" -> [9 nil]; nil for nil; a 400 otherwise,
  by the rules of the lines qualifier (swhid.qualifier)."
  [s]
  (when s
    (try (:lines (q/parse (str "lines=" s)))
         (catch clojure.lang.ExceptionInfo _
           (fail 400 "Invalid lines parameter (expected N or N-M, from 1, N <= M)"
                 {:lines s})))))

(defn qualify
  "`core` (a SWHID map) with the qualifiers its context allows: origin
  when `origin` is public, visit when the archive has a `snapshot` of
  it, `anchor` (a core revision SWHID string) when given, path (`rel`,
  \"/\"-separated from the repository root, \".\" for the root itself,
  which is path \"/\") only with an anchor, and `lines` ([from to])."
  [core {:keys [origin snapshot anchor rel lines]}]
  (let [public? (and origin (origin/public? origin))]
    (cond-> core
      public? (swhid/with-origin origin)
      (and public? snapshot) (swhid/with-visit (swhid/make :snapshot snapshot))
      anchor (swhid/with-anchor anchor)
      (and anchor rel) (swhid/with-path (if (= rel ".") "/" (str "/" rel)))
      lines (swhid/with-lines (first lines) (second lines)))))

(def ^:private anchor-candidates-max
  "Number of the most recent commits that may anchor a SWHID; beyond
  them, only the commit that last changed the artifact may."
  100)

(defn- object-at
  "Hash of the object at `rel` (\".\" for the root directory) in
  `commit` of `repo`, of `type` (\"blob\" or \"tree\") when given; nil
  when it has none."
  [repo commit rel & [type]]
  (when-let [hash (origin/git-out repo "rev-parse" "--verify" "-q"
                                  (if (= rel ".") (str commit "^{tree}") (str commit ":" rel)))]
    (when (or (nil? type) (= type (origin/git-out repo "cat-file" "-t" hash)))
      hash)))

(defn- rev-list
  "Hashes of HEAD's last `anchor-candidates-max` commits in `repo`, most
  recent first, with the git rev-list `args` after HEAD."
  [repo & args]
  (some-> (apply origin/git-out repo "rev-list" (str "--max-count=" anchor-candidates-max)
                 "HEAD" args)
          str/split-lines))

(defn- holds?
  "True when `commit` of `repo` holds the object whose `hash` is given at
  `rel`."
  [repo commit rel hash]
  (= hash (object-at repo commit rel)))

(defn- archived-commits
  "Those of `commits` (hashes, most recent first) the archive holds,
  lazily and in order. The archive is asked once, only about those more
  recent than the first one the cache knows."
  [{:keys [cache known-fn]} commits]
  (let [cached? #(cache/known? cache (revision %))
        unknown (map revision (take-while (complement cached?) commits))
        answer (if (seq unknown) (known-fn unknown) {})]
    (filter #(or (cached? %) (get answer (revision %))) commits)))

(defn- anchor-candidates
  "Commits of HEAD's history that may hold the object whose `hash` is
  given at `rel`, most recent first: an anchor must hold the content the
  identifier is about. From HEAD back to the commit that last changed
  `rel`, the most recent `anchor-candidates-max` and that one; a commit
  merged from another branch may not hold it (see `holds?`). None when
  HEAD does not hold it (an edited or untracked file is not in it)."
  [repo rel hash]
  (when (holds? repo "HEAD" rel hash)
    (when-let [changed (origin/git-out repo "--literal-pathspecs" "log" "-1" "--format=%H"
                                       "HEAD" "--" rel)]
      (distinct (concat (rev-list repo "--not" (str changed "^@")) [changed])))))

(defn- archived-anchor
  "The most recent of the `anchor-candidates` that holds the object and
  the archive holds, as a core revision SWHID string, nil when none:
  HEAD when the archive holds it, else an older commit. Those that do
  not hold it are set aside first: `archived-commits` stops asking the
  archive at the first commit the cache knows, which must then be one
  that holds it, or an older one holding it would never be asked about."
  [ctx repo rel hash]
  (some->> (anchor-candidates repo rel hash)
           (filter #(holds? repo % rel hash))
           (archived-commits ctx)
           first
           revision))

(defn- archived-version
  "The version of the artifact at `rel` the archive holds: that of the
  most recent of HEAD's last `anchor-candidates-max` commits the archive
  holds and that has one of the same kind, a directory when `dir?`, as
  [commit hash]; nil when none."
  [ctx repo rel dir?]
  (some #(some->> (object-at repo % rel (if dir? "tree" "blob")) (vector %))
        (archived-commits ctx (rev-list repo))))

(defn- browse-base
  "Web root of the archive instance `api-opts` points at."
  [api-opts]
  (str/replace (or (:base-url api-opts) api/default-base-url) #"/api/1/?$" ""))

(defn- qualified-swhid
  "The artifact at `path` as a qualified SWHID (see `qualify`), verified
  against the archive: its content, and as anchor the most recent
  commit that holds this content at this path and the archive knows
  (see `archived-anchor`). A directory in a Git repository stands for
  its tracked working tree: a 400 when nothing in it is tracked, before
  the archive is asked.
  With `archived?`, the version the archive holds instead, anchored at
  its commit and without `lines` (see `archived-version`), with that
  commit under :version; the working tree's, with `lines`, when there
  is none, and the 400 of a directory where nothing is tracked only
  then: the archive may hold a version of a directory taken out of the
  index."
  [{:keys [api-opts origin-opts] :as ctx} path lines archived?]
  (let [f (scan/normalize path)
        {:keys [dir? repo tracked?] :as artifact} (scan/artifact path)
        nothing-tracked! #(when (and dir? repo (not tracked?))
                            (fail 400 (str "Nothing is tracked in " f) {:path path}))
        _ (when-not archived? (nothing-tracked!))
        _ (when (and lines dir?)
            (fail 400 "lines apply to a file, not a directory" {:path path}))
        rel (when repo (scan/relpath repo f))
        {:keys [origin visit]} (when repo (origin/lookup repo api-opts origin-opts))
        public? (and origin (origin/public? origin))
        [commit hash] (when (and archived? public?) (archived-version ctx repo rel dir?))
        _ (when-not commit (nothing-tracked!))
        {:keys [swhid archived]} (if commit
                                   {:swhid (swhid/swhid->string
                                            (swhid/make (if dir? :directory :content) hash))
                                    :archived true}
                                   (lookup! ctx artifact))
        core (swhid/parse swhid)
        anchor (cond commit (revision commit)
                     public? (archived-anchor ctx repo rel (:hash core)))
        qualified (swhid/swhid->string
                   (qualify core {:origin origin
                                  :snapshot (:snapshot visit)
                                  :anchor anchor
                                  :rel rel
                                  :lines (when-not commit lines)}))]
    (cond-> {:swhid qualified
             :core swhid
             :archived archived
             :browse-url (str (browse-base api-opts) "/" qualified)}
      origin (assoc :origin origin :origin-archived (some? visit))
      commit (assoc :version anchor))))

;;; Routes

(defn- directory!
  "`path` as a normalized directory, a 400 otherwise."
  [path]
  (let [f (scan/normalize path)]
    (when-not (.isDirectory f)
      (fail 400 "Not a directory" {:path path}))
    f))

(defn- route
  "Response for `request` with its query `params`."
  [{:keys [cache known-fn api-opts origin-opts authenticated] :as ctx}
   {:keys [request-method uri]} params]
  (let [path (get params "path")
        ;; Every route but /ping needs an existing path: 400 otherwise.
        path! (fn []
                (when-not path (fail 400 "Missing path parameter"))
                (when-not (.exists (scan/normalize path))
                  (fail 400 "No such path" {:path path}))
                path)
        dir! #(directory! (path!))]
    (case [request-method uri]
      [:get "/ping"]
      (respond 200 {:pong true :version version :authenticated authenticated})
      [:get "/tree"]
      (respond 200 (with-provenances ctx (scan/scan (dir!) known-fn #(cache/known? cache %))))
      [:get "/file"]
      (respond 200 (select-keys (lookup! ctx (scan/artifact (path!))) [:swhid :archived]))
      [:get "/swhid"]
      (respond 200 (qualified-swhid ctx (path!) (parse-lines (get params "lines"))
                                    (contains? params "archived")))
      [:get "/origin"]
      (let [{:keys [repo origin visit]} (resolve-origin! ctx (path!))
            ;; only an archived origin, hence a public one: the archive
            ;; is not told about the commits of a private repo
            upstream (when (and (contains? params "upstream") visit)
                       (upstream-revision repo))]
        (respond 200 (cond-> (merge {:origin origin :archived (some? visit)}
                                    (select-keys visit [:date :status :snapshot]))
                       upstream (assoc :upstream upstream
                                       :upstream-archived (archived? ctx upstream)))))
      [:get "/visits"]
      ;; n first: resolving the origin may ask the archive
      (let [n (if-let [raw (get params "n")] (parse-long raw) 10)
            _ (when-not (pos-int? n)
                (fail 400 "Invalid n parameter" {:n (get params "n")}))
            {:keys [origin visit]} (resolve-origin! ctx (path!))]
        (respond 200 {:origin origin
                      :visits (if visit
                                (api/visits origin (assoc api-opts :per-page n))
                                [])}))
      [:get "/provenance"]
      (let [s (core-swhid! (get params "swhid"))]
        (respond 200 {:swhid s :provenance (provenance! ctx s)}))
      [:post "/save"]
      ;; save under the URL the archive already knows, if any
      (let [{:keys [origin]} (resolve-origin! ctx (path!))]
        (when-not (origin/public? origin)
          (fail 400 "The origin is not a public URL" {:origin origin}))
        (let [saved (api/save-code-now! origin api-opts)]
          ;; the archive's answer about this origin is about to change
          (origin/forget! (:cache origin-opts) origin)
          (respond 200 (assoc saved :origin origin))))
      (respond 404 {:error "not found"}))))

(defn make-handler
  "Ring-style handler. `api-opts` is passed to swh.api.v1 calls, with
  the token of a request's Authorization header when there is one;
  `log`, when given, receives one line per request (method, route,
  status, duration, error) and per call to the archive. What the
  archive says about an origin is remembered for `origin-ttl-ms`
  (swhd.origin/default-ttl-ms) and forgotten by a save request; that
  it knows no provenance for an artifact, as long. The provenances it
  finds go to `provenance` (swhd.cache/init-provenance), in memory by
  default."
  [{:keys [cache provenance api-opts log origin-ttl-ms]}]
  (let [log (or log (fn [_]))
        visits (atom {})
        provenance (or provenance (cache/init-provenance nil))
        misses (atom {})
        ;; the daemon's own token, which clients without one rely on
        authenticated (some? (:token api-opts))
        ctx-for (fn [api-opts]
                  {:cache cache
                   :authenticated authenticated
                   :provenance provenance
                   :misses misses
                   :ttl-ms (or origin-ttl-ms origin/default-ttl-ms)
                   :log log
                   :api-opts api-opts
                   :known-fn (known-fn cache api-opts log)
                   :origin-opts (cond-> {:cache visits :log log}
                                  origin-ttl-ms (assoc :ttl-ms origin-ttl-ms))})
        ctx (ctx-for api-opts)]
    (fn [{:keys [request-method uri query-string headers] :as req}]
      (let [start (System/nanoTime)
            ;; a client may bring its own token, as swh.el does
            token (bearer-token headers)
            params (query-params query-string)
            {:keys [status body]}
            (try (when-not (local-request? headers)
                   (fail 403 "Forbidden: not a local request"))
                 (route (if token (ctx-for (assoc api-opts :token token)) ctx)
                        req
                        (or params (fail 400 "Malformed query string" {:query query-string})))
                 (catch Exception e (error-response e)))
            ms (quot (- (System/nanoTime) start) 1000000)]
        (log (str (str/upper-case (name request-method)) " " uri
                  (when-let [p (get params "path")] (str " path=" p))
                  " -> " status " (" ms " ms)"
                  (when (>= status 400) (str ": " (:error body)))))
        {:status status
         :headers {"Content-Type" "application/json"}
         :body (json/generate-string body)}))))

(defn env-token
  "The token in the SWH_API_TOKEN value `raw`, nil when it is unset or
  blank: an empty token would be sent as a malformed Authorization
  header, which the archive rejects on every call."
  [raw]
  (when-not (str/blank? raw)
    (str/trim raw)))

(defn- port-from-env
  "SWHD_PORT as a port number, 8642 when unset; exits on a bad value
  rather than listening where the clients do not expect it."
  []
  (if-let [raw (System/getenv "SWHD_PORT")]
    (let [port (parse-long raw)]
      (if (and port (< 0 port 65536))
        port
        (do (log-stderr (str "swhd: invalid SWHD_PORT: " (pr-str raw)))
            (System/exit 1))))
    8642))

(defn -main [& args]
  (when (some #{"--version"} args)
    (println "swhd" version)
    (System/exit 0))
  (let [port (port-from-env)
        token (env-token (System/getenv "SWH_API_TOKEN"))
        handler (make-handler (assoc (cache/init-defaults log-stderr)
                                     :api-opts (when token {:token token})
                                     :log log-stderr))]
    (srv/run-server handler {:port port :ip "127.0.0.1"})
    (log-stderr (str "swhd listening on http://127.0.0.1:" port
                     (when token " (authenticated)")))
    @(promise)))
