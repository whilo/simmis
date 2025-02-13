(ns is.simm.simulations.screenshare.screens
  (:require [clojure.java.shell :as sh]
            [datahike.api :as d]
            [missionary.core :as m]
            [clojure.java.io :as io]
            [cloroutine.impl :as i]))

;; A database for screens per user
;; Contains screen id and whether it is active

(def schema [{:db/ident :screen
              :db/unique :db.unique/identity
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one}
             {:db/ident :user
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one}
             {:db/ident :active
              :db/valueType :db.type/boolean
              :db/cardinality :db.cardinality/one}])

(def screens-conn
  (let [cfg {:store {:backend :mem :id (str "screens")}}]
    (try
      (d/create-database cfg)
      (let [conn (d/connect cfg)]
        (d/transact conn schema)
        conn)
      (catch Exception _e
        (println "Screen database already exists ")
        (d/connect cfg)))))

(defn get-screens [db user]
  (d/q '[:find ?s ?a
         :in $ ?user
         :where
         [?e :screen ?s]
         [?e :user ?user]
         [?e :active ?a]]
       db user))

(defn add-screen! [user screen]
  (d/transact! screens-conn [{:screen screen
                              :user user
                              :active true}])
  nil)

(defn remove-screen! [screen]
  (prn "removing" screen)
  (prn (d/transact! screens-conn [[:db.fn/retractEntity [:screen screen]]]))
  (prn "after remove")
  nil)

(defn set-active! [screen active]
  (d/transact! screens-conn [{:db/id [:screen screen]
                              :active active}])
  nil)

(def !running-rustdesks (atom {}))

;; every 60 seconds get all active screens for all users
;; and check that rustdesk is running for each one
;; run rustdesk in a misisonary task with m/sp
(defn maintain-rustdesk
  "Every 60 seconds check that rustdesk is running for all active screens."
  []
  (m/sp
   (while true
     (prn "maintain-rustdesks")
     (let [screens (d/q '[:find ?s ?u
                          :where
                          [?e :screen ?s]
                          [?e :user ?u]
                          [?e :active true]]
                        @screens-conn)]
       (doseq [[screen _user] screens]
         (when (not (contains? @!running-rustdesks screen))
           (prn "starting rustdesk for" screen)
           (let [process ((m/via m/blk
                                 (prn (sh/sh "rustdesk" "--connect" screen))
                                 (prn "dissoc" (swap! !running-rustdesks dissoc screen))
                                 (prn "running-rustdesks" @!running-rustdesks))
                          prn prn)]
             (prn "process" process)
             (swap! !running-rustdesks assoc screen process)))))
     (m/? (m/sleep 60000)))))

(add-screen! "tester" "1544435271")

(add-screen! "tester" "118756091")

(comment
(defonce rustdesk-task (maintain-rustdesk))

(rustdesk-task prn prn)
)

;; recordings

;; schema, filename, screen, last modified
(def recording-schema [{:db/ident :filename
                        :db/unique :db.unique/identity
                        :db/valueType :db.type/string
                        :db/cardinality :db.cardinality/one}
                       {:db/ident :screen
                        :db/valueType :db.type/string
                        :db/cardinality :db.cardinality/one}
                       {:db/ident :last-modified
                        :db/valueType :db.type/instant
                        :db/cardinality :db.cardinality/one}])

(def recordings-conn
  (let [cfg {:store {:backend :mem :id (str "recordings")}}]
    (try
      (d/create-database cfg)
      (let [conn (d/connect cfg)]
        (d/transact conn recording-schema)
        conn)
      (catch Exception _e
        (println "Recording database already exists ")
        (d/connect cfg)))))

(defn list-recordings [screen]
  (let [files (io/file "/home/christian/Videos/RustDesk")
        recordings (filter #(.contains (.getName %) (str "outgoing_" screen "_")) (.listFiles files))]
    (map #(.getName %) recordings)))

(defn maintain-recordings
  "Every 60 seconds sync recordings with db."
  []
  (m/sp
   (while true
     (prn "maintain-recordings")
     (let [screens (d/q '[:find ?s
                          :where
                          [?e :screen ?s]]
                        @screens-conn)]
       (prn "screens" screens)
       (doseq [[screen] screens]
         (prn "screen" screen)
         (let [recordings (list-recordings screen)
               txs (vec (mapcat
                         (fn [rec]
                           (let [lm' (d/q '[:find ?lm .
                                            :in $ ?filename ?screen
                                            :where
                                            [?e :filename ?f]
                                            [?e :screen ?s]
                                            [?e :last-modified ?lm]]
                                          @recordings-conn rec screen)
                                 lm (java.util.Date. (.lastModified (io/file (str "/home/christian/Videos/RustDesk/" rec))))]
                             (when-not (= lm lm')
                               [{:filename rec
                                 :screen screen
                                 :last-modified lm}])))
                         recordings))]
           (prn screen "txs" txs)
         ;; only update if last modified has changed
           (d/transact! recordings-conn txs))
         (m/? (m/sleep 60000)))))))


(comment
(def recordings-task (maintain-recordings))

(recordings-task prn prn)
)

(comment
;; query all the filenames
  (d/q '[:find ?f
         :where
         [?e :filename ?f]]
       @recordings-conn)

  (d/datoms @recordings-conn :eavt)
  
  )

(defn remove-recording! [filename]
  (let [file (io/file (str "/home/christian/Videos/RustDesk/" filename))]
    (prn "removing" file)
    #_(prn (.delete file))
    nil))

(defn parse-date [filename]
  (let [date-str (nth (.split filename "_") 2)]
    (str (subs date-str 0 4) "-" (subs date-str 4 6) "-" (subs date-str 6 8) " " (subs date-str 8 10) ":" (subs date-str 10 12) ":" (subs date-str 12 14))))


;; extract a frame every 60 seconds from a video file with ffmpeg
;; ffmpeg -i input.mp4 -vf "fps=1/60" frame_%04d.png
(defn process-frames [filename]
  (let [f (.getName (io/file filename))
        output (str "/home/christian/Videos/RustDesk/frames/" 
                    (subs f 0 (.lastIndexOf f ".")) 
                    "_%04d.png")]
    (sh/sh "ffmpeg" "-i" filename "-vf" "fps=1/60" output)))



(comment

  (println (:err (process-frames "/home/christian/Videos/RustDesk/outgoing_1544435271_20241208194232908_display0_vp9.webm")))

  ;; get last access time
  "/home/christian/Videos/RustDesk/outgoing_1544435271_20241208194232908_display0_vp9.webm"
  (let [f (io/file "/home/christian/Videos/RustDesk/outgoing_1544435271_20241208194232908_display0_vp9.webm")]
    (java.util.Date. (.lastModified f)))

  (let [f (io/file "/home/christian/Videos/RustDesk/outgoing_1544435271_20241208194232908_display0_vp9.webm")]
    (prn (.getName f)))

  (.exists (io/file "/home/christian/Videos/RustDesk/outgoing_1544435271_20241208194232908_display0_vp9.webm"))

  (parse-date "outgoing_1544435271_20241208194232908_display0_vp9.webm")

  (list-recordings "1544435271")

  (def rustdesk-task (maintain-rustdesk))

  (rustdesk-task prn prn)


  @!running-rustdesks

  (reset! !running-rustdesks {})



  (d/datoms @screens-conn :eavt)

  (add-screen! "tester" "1544435271")

  (add-screen! "tester" "118756091")


  (remove-screen! "2345")





;; list env vars
  (sh/sh "env")

;; parse this "SHELL=/usr/bin/fish\nSESSION_MANAGER=local/dyson:@/tmp/.ICE-unix/10277,unix/dyson:/tmp/.ICE-unix/10277\nQT_ACCESSIBILITY=1\nSSH_AGENT_LAUNCHER=gnome-keyring\nXDG_MENU_PREFIX=gnome-\nGNOME_DESKTOP_SESSION_ID=this-is-deprecated\nAPPLICATION_INSIGHTS_NO_DIAGNOSTIC_CHANNEL=1\nJACK_IN_NREPL_VERSION=1.1.1\nLANGUAGE=en_CA:en\nJACK_IN_NREPL_PORT_FILE=.nrepl-port\nGNOME_SHELL_SESSION_MODE=ubuntu\nSSH_AUTH_SOCK=/run/user/1000/keyring/ssh\nELECTRON_RUN_AS_NODE=1\nXMODIFIERS=@im=ibus\nDESKTOP_SESSION=ubuntu\nGTK_MODULES=gail:atk-bridge\nPWD=/home/christian/Development/simmis\nXDG_SESSION_DESKTOP=ubuntu\nLOGNAME=christian\nXDG_SESSION_TYPE=wayland\nVSCODE_ESM_ENTRYPOINT=vs/workbench/api/node/extensionHostProcess\nSYSTEMD_EXEC_PID=10300\nVSCODE_CODE_CACHE_PATH=/home/christian/.config/Code/CachedData/f1a4fb101478ce6ec82fe9627c43efbf9e98c813\nOMF_PATH=/home/christian/.local/share/omf\nXAUTHORITY=/run/user/1000/.mutter-Xwaylandauth.904CY2\nGJS_DEBUG_TOPICS=JS ERROR;JS LOG\nHOME=/home/christian\nUSERNAME=christian\nAUTOJUMP_ERROR_PATH=/home/christian/.local/share/autojump/errors.log\nLANG=en_CA.UTF-8\nXDG_CURRENT_DESKTOP=Unity\nVSCODE_IPC_HOOK=/run/user/1000/vscode-2d645b8c-1.95-main.sock\nWAYLAND_DISPLAY=wayland-0\nOSTYPE=linux-gnu\nVSCODE_L10N_BUNDLE_LOCATION=\nINVOCATION_ID=ddf354fb80e3431ba6c6223d8474fdfa\nMANAGERPID=9973\nCHROME_DESKTOP=code.desktop\nJACK_IN_CIDER_NREPL_VERSION=0.47.1\nGJS_DEBUG_OUTPUT=stderr\nGNOME_SETUP_DISPLAY=:1\nXDG_SESSION_CLASS=user\nUSER=christian\nAUTOJUMP_SOURCED=1\nDISPLAY=:0\nVSCODE_PID=12624\nSHLVL=1\nGUIX_LOCPATH=/home/christian/.guix-profile/lib/locale\nQT_IM_MODULE=ibus\nAPPLICATIONINSIGHTS_CONFIGURATION_CONTENT={}\nVSCODE_CWD=/home/christian\nJULIA_HOME=/home/christian/Programs/julia-1.6.1/bin\nVSCODE_CRASH_REPORTER_PROCESS_TYPE=extensionHost\nXDG_RUNTIME_DIR=/run/user/1000\nJOURNAL_STREAM=8:49359\nXDG_DATA_DIRS=/usr/local/share/:/usr/share/:/var/lib/snapd/desktop\nGDK_BACKEND=x11\nPATH=/home/christian/.elan/bin:/home/christian/.cabal/bin:/home/christian/.ghcup/bin:/home/christian/.cabal/bin:/home/christian/Programs/bin:/home/christian/Programs/julia-1.9.2/bin:/home/christian/.local/bin/:/home/christian/.guix-profile/bin:/home/christian/Programs/go/bin:/home/christian/.cabal/bin:/home/christian/.ghcup/bin:/home/christian/.cabal/bin:/home/christian/Programs/bin:/home/christian/Programs/julia-1.9.2/bin:/home/christian/.local/bin/:/home/christian/.guix-profile/bin:/home/christian/Programs/go/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin:/snap/bin\nGDMSESSION=ubuntu\nORIGINAL_XDG_CURRENT_DESKTOP=ubuntu:GNOME\nDBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1000/bus\nVSCODE_NLS_CONFIG={\"userLocale\":\"en-gb\",\"osLocale\":\"en-ca\",\"resolvedLanguage\":\"en\",\"defaultMessagesFile\":\"/usr/share/code/resources/app/out/nls.messages.json\",\"locale\":\"en-gb\",\"availableLanguages\":{}}\nJACK_IN_CLJ_MIDDLEWARE=cider.nrepl/cider-middleware\nOMF_CONFIG=/home/christian/.config/omf\nJACK_IN_CLI_ALIASES=:dev\nGIO_LAUNCHED_DESKTOP_FILE_PID=12624\nGIO_LAUNCHED_DESKTOP_FILE=/usr/share/applications/code.desktop\nVSCODE_HANDLES_UNCAUGHT_ERRORS=true\nJACK_IN_PROJECT_ROOT_PATH=/home/christian/Development/simmis\n",
;; into a map
  (defn parse-env [env]
    (reduce
     (fn [m [k v]]
       (assoc m k v))
     {}
     (map #(clojure.string/split % #"=") (clojure.string/split env #"\n"))))

  (parse-env (:out (sh/sh "env" :env {"DISPLAY" ":99.0"})))


;; run: rustdesk --connect 1544435271

  (sh/sh "rustdesk" "--connect" "1544435271")
  )