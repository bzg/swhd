(ns swhd.core-test
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.test :refer [deftest is testing]]
            [swhd.core :as daemon]
            [swhd.cache :as cache]
            [swhd.scan :as scan]
            [swhd.test-util :refer [temp-dir delete-recursively]]
            [swhid.core :as swhid]
            [swhid.hash :as h]))

(defn- fake-api
  "swh-api :request hook: answers /known/ with `known-set`, /save/ with an
  accepted save request, /visit/latest/ with `visit` (404 when nil, or
  when `visit-origin` is given and the URL asks for another origin)."
  [known-set & {:keys [visit visit-origin]}]
  (fn [{:keys [uri body]}]
    (cond
      (str/ends-with? uri "/known/")
      {:status 200 :headers {}
       :body (json/generate-string
              (into {} (map (fn [s] [s {:known (contains? known-set s)}]))
                    (json/parse-string body)))}
      (str/includes? uri "/origin/save/")
      {:status 200 :headers {}
       :body (json/generate-string {:save_request_status "accepted"})}
      (str/includes? uri "/visits/")
      {:status 200 :headers {}
       :body (json/generate-string
              (if visit
                [visit {:date "2026-06-01T00:00:00+00:00" :status "partial"}]
                []))}
      (str/includes? uri "/visit/latest/")
      (if (and visit
               (or (nil? visit-origin)
                   (str/includes? uri (str "/origin/" visit-origin "/visit/"))))
        {:status 200 :headers {} :body (json/generate-string visit)}
        {:status 404 :headers {} :body "{}"})
      :else (throw (ex-info "unexpected uri" {:uri uri})))))

(defn- handler-for [known-set cache-file & fake-opts]
  (daemon/make-handler {:cache (cache/init cache-file)
                        :api-opts {:request (apply fake-api known-set fake-opts)}}))

(defn- get-json [handler method uri & [path]]
  (let [resp (handler {:request-method method :uri uri
                       :query-string (when path
                                       (str "path=" (java.net.URLEncoder/encode path "UTF-8")))})]
    [(:status resp) (json/parse-string (:body resp) true)]))

(deftest handler-test
  (let [work (temp-dir "swhd-test")
        project (io/file work "project")
        cache-file (str (io/file work "known.log"))]
    (try
      (.mkdir project)
      (spit (io/file project "a.txt") "aaa\n")
      (let [file-swhid (swhid/swhid->string (h/path-swhid (io/file project "a.txt")))
            handler (handler-for #{file-swhid} cache-file)]
        (testing "/ping"
          (is (= [200 {:pong true :version daemon/version :authenticated false}]
                 (get-json handler :get "/ping")))
          (testing "with a token of its own, even to a client bringing one"
            (let [handler (daemon/make-handler {:cache (cache/init cache-file)
                                                :api-opts {:token "own"}})
                  resp (handler {:request-method :get :uri "/ping"
                                 :headers {"authorization" "Bearer client"}})]
              (is (true? (:authenticated (json/parse-string (:body resp) true)))))))
        (testing "/tree"
          (let [[status body] (get-json handler :get "/tree" (str project))]
            (is (= 200 status))
            (is (true? (get-in body [(keyword "a.txt") :archived])))
            ;; the root directory itself is not known
            (is (false? (get-in body [(keyword ".") :archived])))
            (is (str/starts-with? (get-in body [(keyword ".") :swhid]) "swh:1:dir:"))))
        (testing "/tree gives the provenances the cache holds, asking nothing"
          (let [found (str file-swhid ";origin=https://example.org/lib")
                handler (daemon/make-handler
                         {:cache (cache/init cache-file)
                          :provenance (doto (cache/init-provenance nil)
                                        (cache/add-provenance! file-swhid found))
                          :api-opts {:request (fake-api #{file-swhid})}})
                [_ body] (get-json handler :get "/tree" (str project))]
            (is (= found (get-in body [(keyword "a.txt") :provenance])))
            (is (not (contains? (get body (keyword ".")) :provenance)))))
        (testing "/file + cache persistence"
          (let [[status body] (get-json handler :get "/file" (str (io/file project "a.txt")))]
            (is (= 200 status))
            (is (true? (:archived body)))
            (is (= file-swhid (:swhid body))))
          (is (cache/known? (cache/init cache-file) file-swhid)))
        (testing "unknown route"
          (is (= 404 (first (get-json handler :get "/nope"))))))
      (testing "/save uses the origin remote, in the archive's form"
        (sh/sh "git" "-C" (str project) "init" "-q")
        (sh/sh "git" "-C" (str project) "add" "-A")
        (sh/sh "git" "-C" (str project) "remote" "add" "origin" "https://example.org/repo.git")
        (let [handler (handler-for #{} cache-file)
              [status body] (get-json handler :post "/save" (str project))]
          (is (= 200 status))
          (is (= "accepted" (:save_request_status body)))
          (is (= "https://example.org/repo" (:origin body)))))
      (testing "/save keeps the URL the archive already knows"
        (let [handler (handler-for #{} cache-file
                                   :visit {:date "2026-07-01T03:14:15+00:00"}
                                   :visit-origin "https://example.org/repo.git")
              [_ body] (get-json handler :post "/save" (str project))]
          (is (= "https://example.org/repo.git" (:origin body)))))
      (testing "/file on a repo root excludes .git, like /tree"
        (let [handler (handler-for #{} cache-file)
              [_ tree] (get-json handler :get "/tree" (str project))
              [status body] (get-json handler :get "/file" (str project))]
          (is (= 200 status))
          (is (= (get-in tree [(keyword ".") :swhid]) (:swhid body)))))
      (testing "/file is covered by an archived ancestor directory"
        (let [b (io/file project "b.txt")
              _ (spit b "bbb\n")
              _ (sh/sh "git" "-C" (str project) "add" "b.txt")
              b-swhid (swhid/swhid->string (h/path-swhid b))
              root-swhid (swhid/swhid->string (h/path-swhid project scan/exclude-git))
              handler (handler-for #{root-swhid} cache-file)]
          (is (= [200 {:swhid b-swhid :archived true}]
                 (get-json handler :get "/file" (str b))))
          (is (cache/known? (cache/init cache-file) b-swhid))
          (is (cache/known? (cache/init cache-file) root-swhid))))
      (testing "/origin on an archived repo has the last archival date"
        (let [visit {:date "2026-07-01T03:14:15+00:00" :status "full"
                     :snapshot "0000000000000000000000000000000000000042"}
              handler (handler-for #{} cache-file :visit visit)
              [status body] (get-json handler :get "/origin" (str project))]
          (is (= 200 status))
          (is (= "https://example.org/repo" (:origin body)))
          (is (true? (:archived body)))
          (is (= "2026-07-01T03:14:15+00:00" (:date body)))))
      (testing "/origin falls back on the .git form when that is the archived one"
        (let [handler (handler-for #{} cache-file
                                   :visit {:date "2026-07-01T03:14:15+00:00"}
                                   :visit-origin "https://example.org/repo.git")
              [_ body] (get-json handler :get "/origin" (str project))]
          (is (true? (:archived body)))
          (is (= "https://example.org/repo.git" (:origin body)))))
      (testing "/visits lists the origin's visits, most recent first"
        (let [visit {:date "2026-07-01T03:14:15+00:00" :status "full"
                     :snapshot "0000000000000000000000000000000000000042"}
              handler (handler-for #{} cache-file :visit visit)
              [status body] (get-json handler :get "/visits" (str project))]
          (is (= 200 status))
          (is (= "https://example.org/repo" (:origin body)))
          (is (= ["2026-07-01T03:14:15+00:00" "2026-06-01T00:00:00+00:00"]
                 (mapv :date (:visits body)))))
        (is (= [200 {:origin "https://example.org/repo" :visits []}]
               (get-json (handler-for #{} cache-file) :get "/visits" (str project)))
            "no visits when the origin is not archived")
        (let [resp (handler-for #{} cache-file)
              bad (fn [n] (:status (resp {:request-method :get :uri "/visits"
                                          :query-string (str "path=" (java.net.URLEncoder/encode (str project) "UTF-8")
                                                             "&n=" n)})))]
          (is (= 400 (bad "abc")))
          (is (= 400 (bad "0")))))
      (testing "/origin on an unarchived repo"
        (let [handler (handler-for #{} cache-file)
              [status body] (get-json handler :get "/origin" (str project))]
          (is (= 200 status))
          (is (false? (:archived body)))
          (is (nil? (:date body)))))
      (testing "/save without remote is an error"
        (let [no-remote (io/file work "bare")]
          (.mkdir no-remote)
          (sh/sh "git" "-C" (str no-remote) "init" "-q")
          (let [handler (handler-for #{} cache-file)
                [status body] (get-json handler :post "/save" (str no-remote))]
            (is (= 400 status))
            (is (= "No remote" (:error body))))))
      (testing "/save refuses a remote the archive cannot fetch"
        (let [local (io/file work "local")]
          (.mkdir local)
          (sh/sh "git" "-C" (str local) "init" "-q")
          (sh/sh "git" "-C" (str local) "remote" "add" "origin" "/srv/git/repo")
          (let [calls (atom 0)
                handler (daemon/make-handler
                         {:cache (cache/init cache-file)
                          :api-opts {:request (fn [_] (swap! calls inc) {:status 404 :headers {} :body "{}"})}})
                [status body] (get-json handler :post "/save" (str local))]
            (is (= 400 status))
            (is (= "The origin is not a public URL" (:error body)))
            (is (zero? @calls) "no call to the archive for a local remote")
            (is (= [200 {:origin "/srv/git/repo" :archived false}]
                   (get-json handler :get "/origin" (str local))))
            (is (zero? @calls)))))
      (testing "/origin outside a repository, /tree on a file, a malformed query"
        (let [handler (handler-for #{} cache-file)]
          (is (= [400 {:error "Not a Git repository"}]
                 (get-json handler :get "/origin" (str work))))
          (is (= [400 {:error "Not a directory"}]
                 (get-json handler :get "/tree" (str (io/file project "a.txt")))))
          (let [resp (handler {:request-method :get :uri "/file" :query-string "path=%zz"})]
            (is (= 400 (:status resp)))
            (is (= "Malformed query string" (:error (json/parse-string (:body resp) true)))))))
      (testing "the remote is the tracked one, else origin, else the only one"
        (let [repo (io/file work "remotes")
              git (fn [& args] (apply sh/sh "git" "-C" (str repo) args))
              origin-of (fn [] (:origin (second (get-json (handler-for #{} cache-file)
                                                          :get "/origin" (str repo)))))]
          (.mkdir repo)
          (git "init" "-q" "-b" "main")
          (git "remote" "add" "upstream" "https://example.org/upstream.git")
          (is (= "https://example.org/upstream" (origin-of)) "the only remote")
          (git "remote" "add" "origin" "https://example.org/origin.git")
          (is (= "https://example.org/origin" (origin-of)) "origin by default")
          (git "config" "branch.main.remote" "upstream")
          (is (= "https://example.org/upstream" (origin-of)) "the tracked remote wins")))
      (finally (delete-recursively work)))))

(deftest upstream-errors-test
  (let [work (temp-dir "swhd-test")
        cache-file (str (io/file work "known.log"))
        repo (io/file work "repo")]
    (try
      (.mkdir repo)
      (sh/sh "git" "-C" (str repo) "init" "-q")
      (sh/sh "git" "-C" (str repo) "remote" "add" "origin" "https://example.org/repo")
      (testing "an error answered by the archive is a 502 with its status"
        (let [handler (daemon/make-handler
                       {:cache (cache/init cache-file)
                        :api-opts {:request (fn [_] {:status 503 :headers {}
                                                     :body "<html>maintenance</html>"})}})
              [status body] (get-json handler :get "/origin" (str repo))]
          (is (= 502 status))
          (is (= 503 (:upstream-status body)))
          (is (= "SWH API error 503" (:error body)))))
      (testing "the reason given by the archive is part of the error"
        (let [handler (daemon/make-handler
                       {:cache (cache/init cache-file)
                        :api-opts {:request (fn [_] {:status 403 :headers {}
                                                     :body "{\"exception\":\"AuthenticationFailed\",\"reason\":\"Bearer token expired\"}"})}})
              [status body] (get-json handler :get "/origin" (str repo))]
          (is (= 502 status))
          (is (= 403 (:upstream-status body)))
          (is (= "SWH API error 403: Bearer token expired" (:error body)))))
      (testing "daemon errors keep their own status"
        (let [handler (handler-for #{} cache-file)]
          (is (= 400 (first (get-json handler :get "/tree"))))
          (is (= 404 (first (get-json handler :get "/nope"))))
          (doseq [route ["/tree" "/file" "/swhid" "/origin"]]
            (is (= [400 {:error "No such path"}]
                   (get-json handler :get route (str (io/file work "missing"))))
                route))))
      (finally (delete-recursively work)))))

(deftest origin-cache-test
  (let [work (temp-dir "swhd-test")
        cache-file (str (io/file work "known.log"))
        repo (io/file work "repo")
        visits (atom 0)
        request (fn [{:keys [uri]}]
                  (cond
                    (str/includes? uri "/visit/latest/")
                    (do (swap! visits inc)
                        {:status 200 :headers {}
                         :body (json/generate-string {:date "2026-07-01T03:14:15+00:00"})})
                    (str/includes? uri "/origin/save/")
                    {:status 200 :headers {}
                     :body (json/generate-string {:save_request_status "accepted"})}
                    :else (throw (ex-info "unexpected uri" {:uri uri}))))
        make (fn [& [ttl]]
               (daemon/make-handler (cond-> {:cache (cache/init cache-file)
                                             :api-opts {:request request}}
                                      ttl (assoc :origin-ttl-ms ttl))))]
    (try
      (.mkdir repo)
      (sh/sh "git" "-C" (str repo) "init" "-q")
      (sh/sh "git" "-C" (str repo) "remote" "add" "origin" "https://example.org/repo.git")
      (spit (io/file repo "a.txt") "aaa\n")
      (sh/sh "git" "-C" (str repo) "add" "a.txt")
      (testing "the archive is asked once per origin, whatever the route"
        (let [handler (make)]
          (get-json handler :get "/origin" (str repo))
          (get-json handler :get "/origin" (str repo))
          (get-json handler :get "/swhid" (str (io/file repo "a.txt")))
          (is (= 1 @visits))
          (testing "a save request forgets the answer"
            (get-json handler :post "/save" (str repo))
            (is (= 1 @visits))
            (get-json handler :get "/origin" (str repo))
            (is (= 2 @visits)))))
      (testing "the answer expires"
        (let [handler (make 0)]
          (get-json handler :get "/origin" (str repo))
          (get-json handler :get "/origin" (str repo))
          (is (= 4 @visits))))
      (testing "each handler has its own cache"
        (get-json (make) :get "/origin" (str repo))
        (is (= 5 @visits)))
      (finally (delete-recursively work)))))

(deftest provenance-test
  (let [work (temp-dir "swhd-test")
        file (str (io/file work "provenance.log"))
        cnt "swh:1:cnt:94a9ed024d3859793618152ea559a168bbcbb5e2"
        other "swh:1:cnt:0000000000000000000000000000000000000000"
        found (str cnt ";origin=https://example.org/lib;anchor=swh:1:rel:"
                   (apply str (repeat 40 "1")))
        calls (atom 0)
        status (atom 200)
        request (fn [{:keys [uri]}]
                  (swap! calls inc)
                  {:status @status :headers {}
                   :body (if (str/includes? uri (str "/provenance/whereis/" cnt "/"))
                           (json/generate-string found)
                           "null")})
        make (fn [& [ttl]]
               (daemon/make-handler
                (cond-> {:cache (cache/init (str (io/file work "known.log")))
                         :provenance (cache/init-provenance file)
                         :api-opts {:request request}}
                  ttl (assoc :origin-ttl-ms ttl))))
        ask (fn [handler s]
              (let [resp (handler {:request-method :get :uri "/provenance"
                                   :query-string (str "swhid="
                                                      (java.net.URLEncoder/encode s "UTF-8"))})]
                [(:status resp) (json/parse-string (:body resp) true)]))]
    (try
      (testing "found: answered, then from the cache, kept across restarts"
        (let [handler (make)]
          (is (= [200 {:swhid cnt :provenance found}] (ask handler cnt)))
          (is (= [200 {:swhid cnt :provenance found}] (ask handler (str cnt ";lines=1")))
              "qualifiers are dropped")
          (is (= 1 @calls)))
        (is (= [200 {:swhid cnt :provenance found}] (ask (make) cnt)))
        (is (= 1 @calls)))
      (testing "none: remembered for a while only"
        (let [handler (make)]
          (is (= [200 {:swhid other :provenance nil}] (ask handler other)))
          (ask handler other)
          (is (= 2 @calls)))
        (let [handler (make 0)]
          (ask handler other)
          (ask handler other)
          (is (= 4 @calls))))
      (testing "the archive's refusal is an upstream error"
        (reset! status 403)
        (let [[code body] (ask (make) other)]
          (is (= 502 code))
          (is (= 403 (:upstream-status body)))))
      (testing "a missing or invalid SWHID is a 400"
        (is (= 400 (first (get-json (make) :get "/provenance"))))
        (is (= 400 (first (ask (make) "swh:1:cnt:zz")))))
      (finally (delete-recursively work)))))

(def ^:private snapshot "0000000000000000000000000000000000000042")

(deftest qualify-test
  (testing "the shared fixture: swh.el's swh--qualify must agree"
    (doseq [{:keys [name core origin snapshot anchor rel lines swhid]}
            (:qualify (json/parse-string (slurp (io/resource "swhd/fixtures.json")) true))]
      (is (= swhid
             (swhid/swhid->string
              (daemon/qualify (swhid/parse core)
                              {:origin origin :snapshot snapshot :anchor anchor
                               :rel rel :lines (when lines [(first lines) (second lines)])})))
          name))))

(deftest client-token-test
  (let [work (temp-dir "swhd-test")
        a (io/file work "a.txt")
        seen (atom [])
        handler (daemon/make-handler
                 {:cache (cache/init (str (io/file work "known.log")))
                  :api-opts {:token "env-token"
                             :request (fn [{:keys [headers]}]
                                        (swap! seen conj (get headers "Authorization"))
                                        {:status 200 :headers {} :body "{}"})}})
        file! (fn [headers]
                (handler {:request-method :get :uri "/file" :headers headers
                          :query-string (str "path=" (java.net.URLEncoder/encode (str a) "UTF-8"))}))]
    (try
      (spit a "aaa\n")
      (file! {"host" "localhost:8642"})
      (file! {"host" "localhost:8642" "authorization" "Bearer client-token"})
      (is (= ["Bearer env-token" "Bearer client-token"] @seen)
          "the client's token takes precedence over the daemon's")
      (finally (delete-recursively work)))))

(deftest logging-test
  (let [work (temp-dir "swhd-test")
        cache-file (str (io/file work "known.log"))
        project (io/file work "project")
        lines (atom [])]
    (try
      (.mkdir project)
      (spit (io/file project "a.txt") "aaa\n")
      (let [handler (daemon/make-handler {:cache (cache/init cache-file)
                                          :api-opts {:request (fake-api #{})}
                                          :log #(swap! lines conj %)})]
        (get-json handler :get "/ping")
        (get-json handler :get "/tree" (str project))
        (get-json handler :get "/tree")
        (is (re-matches #"GET /ping -> 200 \(\d+ ms\)" (first @lines)))
        (is (some #(re-matches #"known: asking the archive about \d+ SWHIDs" %) @lines))
        (is (some #(re-matches #"GET /tree path=.* -> 200 \(\d+ ms\)" %) @lines))
        (is (= "GET /tree -> 400 (0 ms): Missing path parameter"
               (str/replace (last @lines) #"\(\d+ ms\)" "(0 ms)"))))
      (finally (delete-recursively work)))))

(deftest error-handling-test
  (let [work (temp-dir "swhd-test")
        project (io/file work "project")
        cache-file (str (io/file work "known.log"))]
    (try
      (.mkdir project)
      (spit (io/file project "a.txt") "aaa\n")
      (testing "a missing path parameter is a 400"
        (let [handler (handler-for #{} cache-file)]
          (is (= [400 {:error "Missing path parameter"}]
                 (get-json handler :get "/tree")))
          (is (= [400 {:error "Missing path parameter"}]
                 (get-json handler :post "/save")))))
      (testing "an API rate limit surfaces as a 429 with retry-after"
        (let [handler (daemon/make-handler
                       {:cache (cache/init cache-file)
                        :api-opts {:request (fn [_] {:status 429
                                                     :headers {"retry-after" "60"}
                                                     :body ""})}})]
          (is (= [429 {:error "SWH API rate limit" :retry-after 60}]
                 (get-json handler :get "/tree" (str project))))))
      (testing "positive answers received before a 429 stay in the cache"
        (let [many (io/file work "many")
              _ (.mkdir many)
              _ (dotimes [i 1001] (spit (io/file many (str i ".txt")) (str i "\n")))
              calls (atom 0)
              ;; call 1: the directory (unknown); call 2: 1000 files
              ;; (known); call 3: the last file -> 429
              request (fn [{:keys [body]}]
                        (let [n (swap! calls inc)]
                          (if (= n 3)
                            {:status 429 :headers {"retry-after" "60"} :body ""}
                            {:status 200 :headers {}
                             :body (json/generate-string
                                    (into {} (map (fn [s] [s {:known (= n 2)}]))
                                          (json/parse-string body)))})))
              handler (daemon/make-handler {:cache (cache/init cache-file)
                                            :api-opts {:request request}})
              [status] (get-json handler :get "/tree" (str many))]
          (is (= 429 status))
          (is (= 3 @calls))
          (is (= 1000 (count (:known @(cache/init cache-file)))))))
      (testing "positive answers of the directory round stay when the file round gets a 429"
        (let [sub (io/file project "sub")
              _ (.mkdir sub)
              _ (spit (io/file sub "b.txt") "bbb\n")
              sub-swhid (swhid/swhid->string (h/path-swhid sub))
              calls (atom 0)
              request (fn [{:keys [body]}]
                        (if (= 1 (swap! calls inc))
                          {:status 200 :headers {}
                           :body (json/generate-string
                                  (into {} (map (fn [s] [s {:known (= s sub-swhid)}]))
                                        (json/parse-string body)))}
                          {:status 429 :headers {"retry-after" "60"} :body ""}))
              handler (daemon/make-handler {:cache (cache/init cache-file)
                                            :api-opts {:request request}})
              [status] (get-json handler :get "/tree" (str project))]
          (is (= 429 status))
          (is (= 2 @calls))
          (is (cache/known? (cache/init cache-file) sub-swhid))))
      (testing "an unreachable archive is a 503 with a plain message"
        (let [handler (daemon/make-handler
                       {:cache (cache/init cache-file)
                        :api-opts {:request (fn [_] (throw (java.net.ConnectException.)))}})]
          (is (= [503 {:error "Cannot reach the Software Heritage archive"}]
                 (get-json handler :get "/tree" (str project))))))
      (testing "a network failure is a 503 however deeply it is wrapped"
        (let [handler (daemon/make-handler
                       {:cache (cache/init cache-file)
                        :api-opts {:request (fn [_] (throw (ex-info "wrapped" {}
                                                                    (RuntimeException.
                                                                     (java.net.UnknownHostException.)))))}})]
          (is (= 503 (first (get-json handler :get "/tree" (str project)))))))
      (testing "any other IOException is a 500 with its own message"
        (let [handler (daemon/make-handler
                       {:cache (cache/init cache-file)
                        :api-opts {:request (fn [_] (throw (java.io.IOException.
                                                            "Cannot run program \"git\"")))}})]
          (is (= [500 {:error "Cannot run program \"git\""}]
                 (get-json handler :get "/tree" (str project))))))
      (testing "requests from a browser page are refused"
        (let [handler (handler-for #{} cache-file)
              ping (fn [headers]
                     (:status (handler {:request-method :get :uri "/ping" :headers headers})))]
          (is (= 200 (ping {"host" "127.0.0.1:8642"})))
          (is (= 200 (ping {"host" "localhost:8642"})))
          (is (= 200 (ping nil)) "not from a browser")
          (is (= 403 (ping {"host" "evil.example:8642"})) "DNS rebinding")
          (is (= 403 (ping {"host" "127.0.0.1:8642" "origin" "https://evil.example"}))
              "cross-origin request")))
      (finally (delete-recursively work)))))

(deftest swhid-route-test
  (let [work (temp-dir "swhd-test")
        ;; a fresh cache per case: a SWHID found archived stays so
        fresh-cache (fn [] (str (io/file work (str (gensym "known") ".edn"))))
        repo (io/file work "repo")
        git (fn [& args]
              (let [{:keys [exit err]}
                    (apply sh/sh "git" "-C" (str repo)
                           "-c" "user.name=Test" "-c" "user.email=test@example.org" args)]
                (assert (zero? exit) err)))
        a (io/file repo "a.txt")
        sub (io/file repo "sub")
        b (io/file sub "b.txt")]
    (try
      (.mkdir repo)
      (.mkdir sub)
      (spit a "aaa\nbbb\nccc\n")
      (spit b "bbb\n")
      (git "init" "-q" "-b" "main")
      (git "remote" "add" "origin" "git@example.org:me/repo.git")
      (git "add" "-A")
      (git "commit" "-q" "-m" "one")
      (let [cnt (fn [f] (swhid/swhid->string (h/path-swhid f)))
            head (str/trim (:out (sh/sh "git" "-C" (str repo) "rev-parse" "HEAD")))
            rev (str "swh:1:rev:" head)
            visit {:date "2026-07-01T03:14:15+00:00" :status "full" :snapshot snapshot}
            swhid-of (fn [handler f & [lines]]
                       (let [resp (handler {:request-method :get :uri "/swhid"
                                            :query-string (str "path=" (java.net.URLEncoder/encode (str f) "UTF-8")
                                                               (when lines (str "&lines=" lines)))})]
                         [(:status resp) (json/parse-string (:body resp) true)]))]
        (testing "a tracked file in an archived repository gets every qualifier"
          (let [handler (handler-for #{(cnt a) rev} (fresh-cache) :visit visit)
                [status body] (swhid-of handler a "1-2")]
            (is (= 200 status))
            (is (= (str (cnt a)
                        ";origin=https://example.org/me/repo"
                        ";visit=swh:1:snp:" snapshot
                        ";anchor=" rev
                        ";path=/a.txt;lines=1-2")
                   (:swhid body)))
            (is (= (cnt a) (:core body)))
            (is (true? (:archived body)))
            (is (true? (:origin-archived body)))
            (is (= "https://example.org/me/repo" (:origin body)))
            (is (= (str "https://archive.softwareheritage.org/" (:swhid body))
                   (:browse-url body)))))
        (testing "a single line, and a nested path"
          (let [handler (handler-for #{(cnt b) rev} (fresh-cache) :visit visit)
                [_ body] (swhid-of handler b "1")]
            (is (str/ends-with? (:swhid body) ";path=/sub/b.txt;lines=1"))))
        (testing "no lines: the whole file"
          (let [handler (handler-for #{(cnt a) rev} (fresh-cache) :visit visit)
                [_ body] (swhid-of handler a)]
            (is (str/ends-with? (:swhid body) ";path=/a.txt"))))
        (testing "an unarchived HEAD gives no anchor nor path, and the content may still be archived"
          (let [handler (handler-for #{(cnt a)} (fresh-cache) :visit visit)
                [_ body] (swhid-of handler a "2")]
            (is (= (str (cnt a) ";origin=https://example.org/me/repo;visit=swh:1:snp:" snapshot ";lines=2")
                   (:swhid body)))
            (is (true? (:archived body)))))
        (testing "an unarchived origin: origin qualifier only, nothing archived"
          (let [handler (handler-for #{} (fresh-cache))
                [_ body] (swhid-of handler a)]
            (is (= (str (cnt a) ";origin=https://example.org/me/repo") (:swhid body)))
            (is (false? (:archived body)))
            (is (false? (:origin-archived body)))))
        (testing "a directory has no lines"
          (let [handler (handler-for #{} (fresh-cache))]
            (is (str/starts-with? (:swhid (second (swhid-of handler sub))) "swh:1:dir:"))
            (is (= 400 (first (swhid-of handler sub "1-2"))))))
        (testing "an untracked directory is a 400, as for swh.el's direct backend, before the archive is asked"
          (let [new (io/file repo "new")
                asked (atom false)
                handler (daemon/make-handler {:cache (cache/init (fresh-cache))
                                              :api-opts {:request (fn [_] (reset! asked true))}})]
            (.mkdir new)
            (spit (io/file new "c.txt") "ccc\n")
            (is (= [400 {:error (str "Nothing is tracked in " (scan/normalize new))}]
                   (swhid-of handler new)))
            (is (false? @asked))
            (delete-recursively new)))
        (testing "the root directory is anchored at path /"
          (let [handler (handler-for #{rev} (fresh-cache) :visit visit)]
            (is (str/ends-with? (:swhid (second (swhid-of handler repo))) (str ";anchor=" rev ";path=/")))))
        (testing "malformed lines are a 400"
          (doseq [bad ["x-y" "0" "0-3" "5-3" "1-"]]
            (is (= 400 (first (swhid-of (handler-for #{} (fresh-cache)) a bad))) bad)))
        (testing "outside a repository: the bare content SWHID"
          (let [loose (io/file work "loose.txt")]
            (spit loose "loose\n")
            (let [[status body] (swhid-of (handler-for #{} (fresh-cache)) loose "1")]
              (is (= 200 status))
              (is (= (str (cnt loose) ";lines=1") (:swhid body)))
              (is (nil? (:origin body))))))
        (testing "an edited file is not anchored: HEAD does not hold this content"
          (spit a "edited\n")
          (let [handler (handler-for #{rev} (fresh-cache) :visit visit)
                [_ body] (swhid-of handler a "1")]
            (is (= (str (cnt a) ";origin=https://example.org/me/repo;visit=swh:1:snp:" snapshot ";lines=1")
                   (:swhid body)))
            (is (false? (:archived body))))
          (git "checkout" "-q" "a.txt"))
        (testing "a repository without a remote: no origin qualifiers"
          (git "remote" "remove" "origin")
          (is (= (cnt a) (:swhid (second (swhid-of (handler-for #{} (fresh-cache)) a)))))))
      (finally (delete-recursively work)))))

(deftest request-parsing-test
  (testing "query strings"
    (is (= {} (daemon/query-params nil)))
    (is (= {"path" "/a b" "n" ""} (daemon/query-params "path=%2Fa%20b&n")))
    (is (= {"path" "/tmp/c++"} (daemon/query-params "path=/tmp/c++")) "+ is literal")
    (is (nil? (daemon/query-params "path=%zz")) "malformed"))
  (testing "bearer tokens"
    (is (= "t0k" (daemon/bearer-token {"authorization" "Bearer t0k"})))
    (is (= "t0k" (daemon/bearer-token {"authorization" "bearer  t0k"})))
    (is (nil? (daemon/bearer-token {"authorization" "Basic dXNlcg=="})))
    (is (nil? (daemon/bearer-token {}))))
  (testing "SWH_API_TOKEN"
    (is (= "t0k" (daemon/env-token "t0k")))
    (is (= "t0k" (daemon/env-token " t0k\n")) "trimmed")
    (is (nil? (daemon/env-token nil)) "unset")
    (is (nil? (daemon/env-token "")) "empty, as SWH_API_TOKEN= in an env file")
    (is (nil? (daemon/env-token "  ")) "blank")))
