(ns swhd.origin
  "A repository's origin as the archive knows it: which remote, in which
  form, and its latest archival visit."
  (:require [clojure.string :as str]
            [swh.api.v1 :as api]
            [swhd.git :as git]))

(defn normalize
  "The archive's form of a Git remote URL: https, no user, no trailing
  slash, no \".git\" suffix. ssh:// and git:// URLs and scp-like
  \"git@host:path\" become https URLs; http(s) URLs keep their scheme
  and port. Anything else (a local path, file://) is returned as is."
  [url]
  (let [strip (fn [u]
                (-> u
                    (str/replace #"/+$" "")
                    (str/replace #"\.git$" "")))]
    (if-let [[_ scheme host port path]
             (re-matches #"(?i)(https?|ssh|git|git\+ssh|ssh\+git)://(?:[^@/]+@)?([^/:@]+)(:\d+)?(/.*)" url)]
      ;; ssh:// and git:// are converted to https; their port is the
      ;; ssh/git one, not the web one, and is dropped
      (let [convert? (not (re-matches #"(?i)https?" scheme))]
        (strip (str (if convert? "https" (str/lower-case scheme)) "://"
                    (str/lower-case host) (when-not convert? port) path)))
      (if-let [[_ host path] (re-matches #"(?:[^@/:]+@)?([^@/:]+):(?!//)([^:]+)" url)]
        (strip (str "https://" (str/lower-case host) "/" (str/replace path #"^/+" "")))
        url))))

(defn public?
  "True when `url` is one the archive could fetch."
  [url]
  (boolean (re-find #"^https?://" url)))

(defn- archived-origin
  "`url` (normalized) as the archive knows it: the first of url and
  url.git with a visit (some forges, e.g. cgit, are archived with the
  suffix), with that visit; {:origin url :visit nil} otherwise, without
  asking the archive about a URL it could not fetch."
  [url api-opts]
  (or (when (public? url)
        (some (fn [u]
                (when-let [visit (api/latest-visit u api-opts)]
                  {:origin u :visit visit}))
              [url (str url ".git")]))
      {:origin url :visit nil}))

(def default-ttl-ms
  "How long the archive's answer about an origin is remembered."
  (* 10 60 1000))

(defn forget!
  "Drop what `cache` remembers about `origin` (any form of its URL):
  after a save request, the archive's answer is about to change."
  [cache origin]
  (swap! cache dissoc (normalize origin)))

(defn fresh
  "The answer an `entry` ({:value answer :at epoch-ms}) of the cache
  holds, when it is younger than `ttl-ms` at `now`; nil otherwise."
  [{:keys [value at]} now ttl-ms]
  (when (and at (< (- now at) ttl-ms))
    value))

(defn lookup
  "The repository's origin in the archive: {:origin url :visit visit},
  :visit nil (and :origin the normalized remote) when none of its forms
  is archived; nil when the repository has no remote. opts: {:cache
  atom :ttl-ms n :log f} -- the archive is asked once per remote per
  `ttl-ms` (default-ttl-ms), the answer kept in `cache` ({url {:value
  answer :at epoch-ms}}), and `log` is told when it is asked."
  [path api-opts {:keys [cache ttl-ms log] :or {ttl-ms default-ttl-ms}}]
  (when-let [remote (git/remote-url path)]
    (let [url (normalize remote)
          now (System/currentTimeMillis)]
      (or (fresh (get @cache url) now ttl-ms)
          (do
            (when (public? url)
              (log (str "origin: asking the archive about " url)))
            (let [value (archived-origin url api-opts)]
              (swap! cache assoc url {:value value :at now})
              value))))))
