(ns is.simm.views.main
  (:require  [is.simm.views.tasks :refer [TaskManager]]
             [is.simm.simulations.screenshare.view :refer [ScreenShare]]
             [markdown.core :refer [md-to-html-string]]
             #?(:clj [is.simm.runtimes.ubuntu :refer [assist default-assist-config plaicraft-assist-config]])
             #?(:clj [is.simm.users :refer [get-session add-session! update-session! delete-session! get-user]])
             [hyperfiddle.electric3 :as e]
             [hyperfiddle.electric-dom3 :as dom]
             [hyperfiddle.electric-svg3 :as svg]
             #?(:clj [datahike.api :as d])
             [missionary.core :as m]))

#?(:cljs
   (defonce !view-state (atom :main)))

#?(:cljs (defonce !rotation (atom 0)))

(e/defn Header [session-id]
  (e/client
   (let [user (e/server (get-user session-id))]
     (dom/header (dom/props {:class "bg-gray-400 p-4"})
                 (dom/div (dom/props {:class "container mx-auto flex justify-between items-center"})
                          (dom/div (dom/props {:class "flex items-center"})
                                   (dom/a (dom/props {:class "text-white text-xl font-bold"})
                                          (dom/On-all "click" (fn [_e] (reset! !view-state :main)))
                                          (dom/img (dom/props {:src "/simmis.png" :alt "Logo" :class "h-10" :style {:transform (str "rotate(" @!rotation "deg)")}})))
                                   (dom/text "immis"))
                          (dom/nav (dom/props {:class "flex space-x-4"})
                                   (dom/a (dom/props {:class "text-white" :href "#about"})
                                          (dom/On-all "click" (fn [_e] (reset! !view-state :about)))
                                          (dom/text "About"))
                                   (dom/a (dom/props {:class "text-white" :href "#assist"})
                                          (dom/On-all "click" (fn [_e] (reset! !view-state :assist)))
                                          (dom/text "Assist"))
                                   #_(dom/a (dom/props {:class "text-white" :href "#taskmanager"})
                                            (dom/On-all "click" (fn [_e] (reset! !view-state :taskmanager)))
                                            (dom/text "Task Manager"))
                                   (dom/a (dom/props {:class "text-white" :href "#screenshare"})
                                          (dom/On-all "click" (fn [_e] (reset! !view-state :screenshare)))
                                          (dom/text "Screen Share"))
                                   (dom/a (dom/props {:class "text-white" :href "#user"})
                                          (dom/On-all "click" (fn [_e] (js/alert "not implemented")))
                                          (dom/text user))))))))


(e/defn Footer []
  (e/client
   (dom/footer (dom/props {:class "bg-gray-200 p-4"})
               (dom/div (dom/props {:class "container mx-auto flex justify-center text-center"})
                        (dom/p (dom/text "Copyright Christian Weilbach 2024-2025. Powered by ")
                               (dom/a (dom/props {:href "https://clojure.org/" :target "_blank" :class "text-blue-400"})
                                      (dom/text "Clojure"))
                               (dom/text ", ")
                               (dom/a (dom/props {:href "https://datahike.io" :target "_blank" :class "text-blue-400"})
                                      (dom/text "Datahike"))
                               (dom/text " and ")
                               (dom/a (dom/props {:href "https://hyperfiddle.net/" :target "_blank" :class "text-blue-400"})
                                      (dom/text "Electric"))
                               (dom/text "."))))))




(e/defn LoginOrSignup []
  (e/client
   (let [submit-fn (fn [e]
                     (.preventDefault e)
                     (let [user (.-value (.getElementById js/document "user"))
                           password (.-value (.getElementById js/document "password"))]))]
     (dom/div (dom/props {:class "flex flex-col items-center justify-center"})
              (dom/a (dom/props {:class "bg-blue-500 text-white font-bold rounded"
                                 ::dom/href "/auth"}) (dom/text "Login")))
     )))

#?(:cljs (def !assistant-on (atom false)))

#?(:clj (def !assistant-task (atom nil)))

(comment
  (@!assistant-task)
  
  )

(e/defn Markdown [?md-str]
  (e/client
   (let [html (e/server (some-> ?md-str md-to-html-string))]
     (set! (.-innerHTML dom/node) html))))

(e/defn AssistantEvent [event]
  (e/client
   (let [role (get event :event/role)
         audio-in (get event :audio/in)
         audio-out (get event :audio/out)
         assistant-output (get event :assistant/output)
         screen-file (get event :screen/file)
         screen-transcript (get event :screen/transcript)
         youtube-summary (get event :youtube/summary)
         action (get event :action)
         created (get event :event/created)
         timestamp (-> created
                       str
                       #_(subs 16 24))] ; Extract just the time part HH:MM:SS
     (case (get event :event/type)
       :is.simm.runtimes.ubuntu/audio-in
       (dom/div (dom/props {:class "max-w-4xl w-full p-4 mb-3 bg-blue-50 border-l-4 border-blue-500 rounded-lg shadow-sm"})
                (dom/div (dom/props {:class "flex items-start"})
                         #_(dom/div (dom/props {:class "flex-shrink-0 mr-3"})
                                  (svg/svg (dom/props {:xmlns "http://www.w3.org/2000/svg" :class "h-6 w-6 text-blue-500" :fill "none" :viewBox "0 0 24 24" :stroke "currentColor"})
                                           (dom/props {:stroke-width "2" :d "M19 11a7 7 0 01-7 7m0 0a7 7 0 01-7-7m7 7v4m0 0H8m4 0h4m-4-8a3 3 0 01-3-3V5a3 3 0 116 0v6a3 3 0 01-3 3z"})))
                         (dom/div (dom/props {:class "w-full"})
                                  (dom/div (dom/props {:class "flex justify-between"})
                                           (dom/h3 (dom/props {:class "font-semibold text-sm text-blue-700"}) (dom/text "User Input"))
                                           (dom/span (dom/props {:class "text-xs text-gray-500"}) (dom/text timestamp)))
                                  (dom/p (dom/props {:class "mt-1 text-gray-800"}) (dom/text audio-in)))))

       :is.simm.runtimes.ubuntu/audio-out
       (dom/div (dom/props {:class "max-w-4xl w-full p-4 mb-3 bg-green-50 border-l-4 border-green-500 rounded-lg shadow-sm"})
                (dom/div (dom/props {:class "flex items-start"})
                         #_(dom/div (dom/props {:class "flex-shrink-0 mr-3"})
                                  (svg/svg (dom/props {:xmlns "http://www.w3.org/2000/svg" :class "h-6 w-6 text-green-500" :fill "none" :viewBox "0 0 24 24" :stroke "currentColor"})
                                           (dom/props {:stroke-width "2" :d "M9.663 17h4.673M12 3v1m6.364 1.636l-.707.707M21 12h-1M4 12H3m3.343-5.657l-.707-.707m2.828 9.9a5 5 0 117.072 0l-.548.547A3.374 3.374 0 0014 18.469V19a2 2 0 11-4 0v-.531c0-.895-.356-1.754-.988-2.386l-.548-.547z"})))
                         (dom/div (dom/props {:class "w-full"})
                                  (dom/div (dom/props {:class "flex justify-between"})
                                           (dom/h3 (dom/props {:class "font-semibold text-sm text-green-700"}) (dom/text "Audio Response"))
                                           (dom/span (dom/props {:class "text-xs text-gray-500"}) (dom/text timestamp)))
                                  (dom/p (dom/props {:class "mt-1 text-gray-800"}) (dom/text audio-out)))))

       :is.simm.runtimes.ubuntu/assistant-output
       (dom/div (dom/props {:class "max-w-4xl w-full p-4 mb-3 bg-purple-50 border-l-4 border-purple-500 rounded-lg shadow-sm"})
                (dom/div (dom/props {:class "flex items-start"})
                         #_(dom/div (dom/props {:class "flex-shrink-0 mr-3"})
                                  (svg/svg (dom/props {:xmlns "http://www.w3.org/2000/svg" :class "h-6 w-6 text-purple-500" :fill "none" :viewBox "0 0 24 24" :stroke "currentColor"})
                                           (dom/props {:stroke-width "2" :d "M8 10h.01M12 10h.01M16 10h.01M9 16H5a2 2 0 01-2-2V6a2 2 0 012-2h14a2 2 0 012 2v8a2 2 0 01-2 2h-5l-5 5v-5z"})))
                         (dom/div (dom/props {:class "w-full"})
                                  (dom/div (dom/props {:class "flex justify-between"})
                                           (dom/h3 (dom/props {:class "font-semibold text-sm text-purple-700"}) 
                                                   (dom/text (str (if role (str "[" role "] ") "") "Assistant Output")))
                                           (dom/span (dom/props {:class "text-xs text-gray-500"}) (dom/text timestamp)))
                                  (dom/p (dom/props {:class "mt-1 text-gray-800 whitespace-pre-wrap"}) 
                                         #_(dom/text assistant-output)
                                         (Markdown assistant-output)))))

       :is.simm.runtimes.ubuntu/screen-transcript
       (dom/div (dom/props {:class "max-w-4xl w-full p-4 mb-3 bg-amber-50 border-l-4 border-amber-500 rounded-lg shadow-sm"})
                (dom/div (dom/props {:class "flex items-start"})
                         #_(dom/div (dom/props {:class "flex-shrink-0 mr-3"})
                                  (svg/svg (dom/props {:xmlns "http://www.w3.org/2000/svg" :class "h-6 w-6 text-amber-500" :fill "none" :viewBox "0 0 24 24" :stroke "currentColor"})
                                           (dom/props {:stroke-width "2" :d "M9.75 17L9 20l-1 1h8l-1-1-.75-3M3 13h18M5 17h14a2 2 0 002-2V5a2 2 0 00-2-2H5a2 2 0 00-2 2v10a2 2 0 002 2z"})))
                         (dom/div (dom/props {:class "w-full"})
                                  (dom/div (dom/props {:class "flex justify-between"})
                                           (dom/h3 (dom/props {:class "font-semibold text-sm text-amber-700"}) (dom/text "Screen Transcript"))
                                           (dom/span (dom/props {:class "text-xs text-gray-500"}) (dom/text timestamp)))
                                  (dom/p (dom/props {:class "mt-1 text-gray-800"}) (dom/text screen-transcript)))))

       :is.simm.runtimes.ubuntu/screenshot
       (dom/div (dom/props {:class "max-w-4xl w-full p-4 mb-3 bg-green-50 border-l-4 border-green-500 rounded-lg shadow-sm"})
                (dom/div (dom/props {:class "flex items-start"})
                         #_(dom/div (dom/props {:class "flex-shrink-0 mr-3"})
                                  (svg/svg (dom/props {:xmlns "http://www.w3.org/2000/svg" :class "h-6 w-6 text-green-500" :fill "none" :viewBox "0 0 24 24" :stroke "currentColor"})
                                           (dom/props {:stroke-width "2" :d "M9.75 17L9 20l-1 1h8l-1-1-.75-3M3 13h18M5 17h14a2 2 0 002-2V5a2 2 0 00-2-2H5a2 2 0 00-2 2v10a2 2 0 002 2z"})))
                         (dom/div (dom/props {:class "w-full"})
                                  (dom/div (dom/props {:class "flex justify-between"})
                                           (dom/h3 (dom/props {:class "font-semibold text-sm text-green-700"}) (dom/text "Screenshot"))
                                           (dom/span (dom/props {:class "text-xs text-gray-500"}) (dom/text timestamp)))
                                  (dom/img (dom/props {:src (.replace screen-file "resources/public/electric_starter_app" "") })))))

       :is.simm.runtimes.ubuntu/youtube-summary
       (dom/div (dom/props {:class "max-w-4xl w-full p-4 mb-3 bg-red-50 border-l-4 border-red-500 rounded-lg shadow-sm"})
                (dom/div (dom/props {:class "flex items-start"})
                         #_(dom/div (dom/props {:class "flex-shrink-0 mr-3"})
                                  (svg/svg (dom/props {:xmlns "http://www.w3.org/2000/svg" :class "h-6 w-6 text-red-500" :fill "none" :viewBox "0 0 24 24" :stroke "currentColor"})
                                           (dom/props {:stroke-width "2" :d "M15 10l4.553-2.276A1 1 0 0121 8.618v6.764a1 1 0 01-1.447.894L15 14M5 18h8a2 2 0 002-2V8a2 2 0 00-2-2H5a2 2 0 00-2 2v8a2 2 0 002 2z"})))
                         (dom/div (dom/props {:class "w-full"})
                                  (dom/div (dom/props {:class "flex justify-between"})
                                           (dom/h3 (dom/props {:class "font-semibold text-sm text-red-700"}) (dom/text "YouTube Summary"))
                                           (dom/span (dom/props {:class "text-xs text-gray-500"}) (dom/text timestamp)))
                                  (dom/p (dom/props {:class "mt-1 text-gray-800"}) (dom/text youtube-summary)))))

       ;; Default case for unknown event types
       (dom/div (dom/props {:class "max-w-4xl w-full p-4 mb-3 bg-gray-50 border-l-4 border-gray-500 rounded-lg shadow-sm"})
                (dom/div (dom/props {:class "flex items-start"})
                         #_(dom/div (dom/props {:class "flex-shrink-0 mr-3"})
                                  (svg/svg (dom/props {:xmlns "http://www.w3.org/2000/svg" :class "h-6 w-6 text-gray-500" :fill "none" :viewBox "0 0 24 24" :stroke "currentColor"})
                                           (dom/props {:stroke-width "2" :d "M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z"})))
                         (dom/div (dom/props {:class "w-full"})
                                  (dom/div (dom/props {:class "flex justify-between"})
                                           (dom/h3 (dom/props {:class "font-semibold text-sm text-gray-700"}) 
                                                   (dom/text (str "Unknown Event: " (get event :event/type))))
                                           (dom/span (dom/props {:class "text-xs text-gray-500"}) (dom/text timestamp)))
                                  (dom/p (dom/props {:class "mt-1 text-gray-800"}) 
                                         (dom/text (str (when action (str "Action: " action))))))))))))


#?(:cljs (def !user-input (atom "")))
#?(:cljs (def !submitting (atom false)))

(e/defn Assist [config _session]
  (e/server
   (let [db (e/watch (d/connect (:db config)))
         msgs (->> db
                   (d/q '[:find (pull ?e [:*]) :where [?e :event/created ?c]])
                   (map first)
                   (sort-by :event/created)
                   reverse
                   (take 1000))]
     (e/client
      (let [assistant-on (e/watch !assistant-on)]
        (dom/section (dom/props {:class "flex items-center justify-center p-6"})
                     (dom/div (dom/props {:class "max-w-4xl w-full p-4 bg-gray-100 rounded-lg space-y-4"})

                              ;; Compact header with toggle switch
                              (dom/div (dom/props {:class "flex justify-between items-center border-b border-gray-200 pb-3 mb-2"})
                                       ;; Left side: Title
                                       (dom/h1 (dom/props {:class "text-2xl font-bold text-gray-800"})
                                               (dom/text "Assistant"))

                                       ;; Right side: Toggle switch with status indicator
                                       (dom/div (dom/props {:class "flex items-center gap-2"})
                                                (dom/span (dom/props {:class (str "text-sm font-medium "
                                                                                  (if assistant-on
                                                                                    "text-green-600"
                                                                                    "text-gray-500"))})
                                                          (dom/text (if assistant-on "Active" "Inactive")))

                                                ;; Modern toggle button
                                                (when-some [t (dom/button
                                                               (dom/props {:class (str "relative inline-flex h-6 w-11 items-center rounded-full transition-colors focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-offset-1 "
                                                                                       (if assistant-on "bg-blue-500" "bg-gray-300"))})

                                                 ;; Toggle button slider
                                                               (dom/span (dom/props {:class (str "inline-block h-4 w-4 transform rounded-full bg-white transition-transform "
                                                                                                 (if assistant-on "translate-x-6" "translate-x-1"))})
                                                                         (dom/text ""))

                                                 ;; Click event
                                                               (let [res (dom/On "click"
                                                                                 (fn [e]
                                                                                   (.preventDefault e)
                                                                                   (.stopPropagation e)
                                                                                   (swap! !assistant-on not))
                                                                                 nil)
                                                                     [t err] (e/Token res)]
                                                                 t))]
                                                 ;; Server-side effect to start/stop assistant
                                                  (when-some [res (e/server
                                                                   (if assistant-on
                                                                     (do
                                                                       (swap! !assistant-task (fn [task]
                                                                                                #_(assert (nil? task) "Task is not nil.")
                                                                                                (let [a (assist config)]
                                                                                                  (a #(prn ::output %) #(prn ::error %)))))
                                                                       nil)
                                                                     (swap! !assistant-task (fn [dispose!]
                                                                                              (when dispose! (dispose!))
                                                                                              nil))))]
                                                    (prn res)
                                                    (t)))))

                                       ;; ChatGPT-like input box
                                       (dom/div (dom/props {:class "border-t border-gray-200 pt-4"})
                                                (dom/form
                                                 (dom/props {:class "flex flex-col gap-2"})
                                                 (dom/div (dom/props {:class "relative flex items-center"})
                                                 ;; Text input
                                                          (dom/textarea
                                                           (dom/props {:class "w-full p-3 pr-12 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500 resize-none"
                                                                       :rows "3"
                                                                       :placeholder "(WIP) Ask a question or enter a command..."
                                                                       :disabled (not assistant-on)
                                                                       :value (e/watch !user-input)})
                                                           #_(dom/on "input" (fn [e] (reset! !user-input (.. e -target -value)))))

                                                 ;; Submit button
                                                          (dom/button
                                                           (dom/props {:class (str "absolute right-2 bottom-2 p-2 rounded-full "
                                                                                   (if (and assistant-on (not (empty? (e/watch !user-input))) (not (e/watch !submitting)))
                                                                                     "bg-blue-500 text-white hover:bg-blue-600"
                                                                                     "bg-gray-200 text-gray-400 cursor-not-allowed"))
                                                                       :type "submit"
                                                                       :disabled (or (not assistant-on)
                                                                                     (empty? (e/watch !user-input))
                                                                                     (e/watch !submitting))})
                                                           #_(dom/svg (dom/props {:xmlns "http://www.w3.org/2000/svg"
                                                                                :class "h-5 w-5"
                                                                                :viewBox "0 0 20 20"
                                                                                :fill "currentColor"})
                                                                    (dom/props {:d "M10.894 2.553a1 1 0 00-1.788 0l-7 14a1 1 0 001.169 1.409l5-1.429A1 1 0 009 15.571V11a1 1 0 112 0v4.571a1 1 0 00.725.962l5 1.428a1 1 0 001.17-1.408l-7-14z"}))
                                                           #_(dom/on "click"
                                                                   (fn [e]
                                                                     (.preventDefault e)
                                                                     (.stopPropagation e)
                                                                     (let [input-text (e/watch !user-input)]
                                                                       (when (and assistant-on (not (empty? input-text)))
                                                                         (reset! !submitting true)
                                                                         (reset! !user-input "")
                                                                ;; Send the user input to the server
                                                                         (e/server
                                                                          (let [event-data {:event/type :is.simm.runtimes.ubuntu/audio-in
                                                                                            :audio/in input-text
                                                                                            :event/created (java.util.Date.)}
                                                                                db-conn (d/connect (:db config))]
                                                                            (d/transact db-conn [event-data])))
                                                                         (js/setTimeout #(reset! !submitting false) 500)))))))))

                              ;; Status message when no events
                              (when (empty? msgs)
                                (dom/div (dom/props {:class "py-8 text-center text-gray-500"})
                                         (dom/p (dom/text "No events to display"))
                                         (when-not assistant-on
                                           (dom/p (dom/props {:class "text-sm mt-2"})
                                                  (dom/text "Toggle assistant to active to start recording events")))))

                              ;; Event list with a subtle separator between header and content
                              (dom/div (dom/props {:class "space-y-3"})
                                       (e/for [event (e/diff-by hash msgs)]
                                         (AssistantEvent event))))))))))


(e/defn About []
  (e/client
   (dom/section (dom/props {:class "flex items-center justify-center p-6"})
                (dom/div (dom/props {:class "max-w-4xl w-full p-8 bg-gray-100 rounded-lg text-center space-y-8"})
                         
                         ;; Title - centered
                         (dom/h1 (dom/props {:class "text-5xl font-extrabold text-gray-800"}) (dom/text "About"))

                         ;; Mission Section
                         (dom/h2 (dom/props {:class "text-3xl font-semibold text-gray-800"}) (dom/text "Our Mission"))
                         (dom/p (dom/props {:class "text-lg text-gray-700 text-left"}) 
                                (dom/text "Our mission is to create simple and effective task management solutions that help users stay organized and productive."))

                         ;; Features Section
                         (dom/h3 (dom/props {:class "text-2xl font-semibold text-gray-800"}) (dom/text "Features"))
                         (dom/p (dom/props {:class "text-lg text-gray-700 text-left"}) 
                                (dom/text "This application comes with a variety of features designed to improve your workflow, such as task categorization, priority levels, and deadlines."))

                         ;; Key Features List - centered
                         (dom/h4 (dom/props {:class "text-xl font-semibold text-gray-800"}) (dom/text "Key Features"))
                         (dom/ul (dom/props {:class "list-disc pl-6 space-y-2 text-left"})
                                 (dom/li (dom/props {:class "text-lg text-gray-700"}) (dom/text "Task prioritization"))
                                 (dom/li (dom/props {:class "text-lg text-gray-700"}) (dom/text "Deadline management"))
                                 (dom/li (dom/props {:class "text-lg text-gray-700"}) (dom/text "Task categorization"))
                                 (dom/li (dom/props {:class "text-lg text-gray-700"}) (dom/text "Collaborative task sharing"))
                                 (dom/li (dom/props {:class "text-lg text-gray-700"}) (dom/text "Progress tracking")))

                         ;; Additional Information
                         (dom/p (dom/props {:class "text-lg text-gray-700 text-left"}) 
                                (dom/text "With these features, our application provides an intuitive and powerful tool to help manage both simple and complex tasks."))

                         ;; Learn More Section
                         (dom/h3 (dom/props {:class "text-2xl font-semibold text-gray-800"}) (dom/text "Learn More"))
                         (dom/p (dom/props {:class "text-lg text-gray-700 text-left"}) 
                                (dom/text "You can learn more about our mission and features by visiting the following links:"))

                         ;; List of Links - centered
                         (dom/ul (dom/props {:class "list-inside space-y-2 text-left"})
                                 (dom/li (dom/props {:class "text-lg text-blue-600"}) (dom/a (dom/props {:href "https://www.example.com/mission" :class "hover:underline"}) (dom/text "Our Mission")))
                                 (dom/li (dom/props {:class "text-lg text-blue-600"}) (dom/a (dom/props {:href "https://www.example.com/features" :class "hover:underline"}) (dom/text "Our Features")))
                                 (dom/li (dom/props {:class "text-lg text-blue-600"}) (dom/a (dom/props {:href "https://www.example.com/contact" :class "hover:underline"}) (dom/text "Contact Us"))))

                         ;; Final Paragraph
                         (dom/p (dom/props {:class "text-lg text-gray-700 text-left"}) 
                                (dom/text "Feel free to reach out if you have any questions or need support. We're always happy to help!"))))))


(e/defn Main [ring-request]
  (let [view-state (e/client (e/watch !view-state))
        ;session-id (e/server (get-in ring-request [:headers "sec-websocket-key"]))
        session (e/server (get-session ring-request))]
    (e/client
     (binding [dom/node js/document.body]
       (dom/section (dom/props {:class "flex flex-col min-h-screen"})
                    (Header session)
                    (if-not session
                      (LoginOrSignup)
                      (dom/div (dom/props {:class "flex-grow"})
                               (case view-state
                                 :main (ScreenShare session)
                                 :assist (Assist plaicraft-assist-config session)
                                 :screenshare (ScreenShare session)
                                 ;:taskmanager (TaskManager.)
                                 ;:login (Login.)
                                 :about (About))))
                    (Footer))))))