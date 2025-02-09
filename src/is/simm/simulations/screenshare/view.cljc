(ns is.simm.simulations.screenshare.view
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            #?(:clj [is.simm.simulations.screenshare.screens :refer [screens-conn get-screens add-screen! remove-screen! set-active!
                                                                     !running-rustdesks list-recordings remove-recording! parse-date
                                                                     recordings-conn]])
            #?(:clj [datahike.api :as d])
            #?(:clj [is.simm.users :refer [get-user]])))

#?(:cljs
   (def !screen-id (atom "")))

#?(:cljs
   (def !selected-screen (atom nil)))

(e/defn ScreenShare [session]
  (e/server
   (let [user (get-user session)
         db (e/watch screens-conn)
         running-screens (set (keys (e/watch !running-rustdesks)))
         screens (get-screens db user)]
     (prn "server screen share" user db screens running-screens)
     (e/client
      (let [selected-screen (e/watch !selected-screen)
        ;; screen-id (atom "")  ;; Track the current input value
        ;;  add-screen (fn [] (when (not (empty? @screen-id))
        ;;                      (swap! screens conj @screen-id)
        ;;                      (reset! screen-id "")))
]  ;; Add screen to the list and reset input
        (prn "screen-id" @!screen-id user)
        (dom/div (dom/props {:class "flex flex-col items-center justify-center bg-gray-100 p-6"})
                 (dom/div (dom/props {:class "max-w-2xl w-full p-8 bg-white rounded-lg shadow-md text-center space-y-8"})

                       ;; Title and description
                          (dom/h1 (dom/props {:class "text-4xl font-bold mb-4 text-gray-800"}) (dom/text "Screen Share"))
                          (dom/p (dom/props {:class "text-lg font-medium text-gray-700 mb-2"})
                                 (dom/text "Share your screens with simmis."))
                          (dom/p (dom/props {:class "text-gray-600 text-left"})
                                 (dom/text "Simmis will archive your work and retrieve information from your archive for you. It will also use your screen data to support you in the background. You need to install ")
                                 (dom/a (dom/props {:class "text-blue-500 underline" :href "https://rustdesk.com/" :target "_blank"}) (dom/text "Rustdesk"))
                                 (dom/text " on each device you want to screen share with. Enter the screen id shown in Rustdesk below."))

                       ;; Input form for adding screen-id
                          (dom/div (dom/props {:class "mb-4"})
                                   (dom/input (dom/props {:class "border p-2 mr-4 text-gray-700"
                                                          :placeholder "Enter RustDesk screen id"
                                                          :value @!screen-id}))
                                   (dom/On-all "change" (e/fn [e] (.log js/console e) (reset! !screen-id (.. e -target -value))))
                                   (dom/button (dom/props {:class "bg-blue-500 text-white p-2 rounded"})
                                               (dom/On-all "click" (e/fn [e]
                                                                 (prn "click" e)
                                                                 (.log js/console e)
                                                                 (let [screen-id (e/client @!screen-id)]
                                                                   (if (and (>= (count screen-id) 9)
                                                                            (re-matches #"\d+" screen-id))
                                                                     (do
                                                                       (e/server (add-screen! user screen-id))
                                                                       (e/client (reset! !screen-id "")))
                                                                     (js/alert "Invalid RustDesk screen id. It has at least 9 numbers.")))))
                                               (dom/text "Add Screen")))

                       ;; List of screens
                          (if-not (seq screens)  ;; Only show the list if there are screens
                            (dom/p (dom/props {:class "text-gray-600"})
                                   (dom/text "No screens added."))
                            (dom/div (dom/props {:class "space-y-4"})
                                     (dom/h3 (dom/props {:class "text-2xl font-semibold text-gray-800"}) (dom/text "Active Screens"))
                                     (dom/ul (dom/props {:class "list-disc pl-6 space-y-2 text-left"})
                                             (e/for [[screen active] screens]
                                               (dom/li (dom/props {:class "flex items-center space-x-4"})
                                                       (dom/button (dom/props (merge {:class (str "bg-gray-900 text-gray-700 p-2 rounded "
                                                                                                  (if (contains? running-screens screen)
                                                                                                    "text-red-500" "text-gray-700"))}))
                                                                   (dom/On-all "click" (e/fn [e]
                                                                                     (prn "screen" e)
                                                                                     (reset! !selected-screen screen)))
                                                                   (dom/text screen))
                                                    ;; Remove button
                                                       (dom/button (dom/props {:class "bg-red-500 text-white p-2 rounded"})
                                                                   (dom/On-all "click" (e/fn [e]
                                                                                     (prn "remove" e)
                                                                                     (e/server (remove-screen!  screen))))
                                                                   (dom/text "Remove"))
                                                    ;; Activate button
                                                       (dom/button (dom/props {:class "bg-green-500 text-white p-2 rounded"})
                                                                   (dom/On-all "click" (e/fn [e]
                                                                                     (prn "de/activate" e)
                                                                                     (e/server (set-active! screen (not active)))))
                                                                   (if active
                                                                     (dom/text "Deactivate")
                                                                     (dom/text "Activate"))))))))

                          (when selected-screen
                            (dom/h3 (dom/props {:class "text-2xl font-semibold text-gray-800"}) (dom/text (str "Screen " selected-screen)))
                            ;; list recordings for screen
                            (e/server
                             (let [recordings-db (e/watch recordings-conn)
                                   recordings (d/q '[:find [?f ...]
                                                     :in $ ?screen
                                                     :where
                                                     [?e :screen ?screen]
                                                     [?e :filename ?f]]
                                                   recordings-db selected-screen)]
                               (e/client
                                (when (seq recordings)
                                  (dom/h4 (dom/props {:class "text-xl font-semibold text-gray-800"}) (dom/text "Recordings"))
                                  (dom/ul (dom/props {:class "list-disc pl-6 space-y-2 text-left"})
                                          (e/for [rec (seq recordings)]
                                            (dom/li (dom/props {})
                                                    (dom/text (e/server (parse-date rec)))
                                                    (dom/button (dom/props {:class "bg-red-500 text-white p-2 rounded"})
                                                                (dom/On-all "click" (e/fn [e]
                                                                                  (prn "remove recording" e)
                                                                                  (e/server (remove-recording! rec))))
                                                                (dom/text "Remove"))
                                                    (dom/video (dom/props {:src (str "/videos/" rec) :controls true})))))))))))))))))


(comment
  (d/q '[:find [?rec ...] :in $ ?screen :where [?e :screen ?screen] [?e :recording ?rec]]
       @recordings-conn "1544435271")

  )
