(ns cchelper.core
  "Utility program to help with CharaChorder"
  (:require [jsonista.core :as json]
            [seesaw.core :as ss]
            [seesaw.mig :as mig]
            [bagotricks :refer [std-keyword re-get to-long fold-into-vec fold-into-lazy-seq dump-lines-to-file read-lines split-tsv fn->> fn-> <- thread okay-string?]]
            )
  (:gen-class))


;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ;
;; Params and State
;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ;

;; Define the default filename to use to write the log index to
(def default-index-filename
  "cchelper.json")

(defonce textarea-link
  (atom nil))

(defonce logging-on
  (atom nil))

(defonce debug-on
  (atom false))

(defonce word-freq-table
  (atom nil))

(def event-queue-size
  10000)

(def word-queue-size
  1000)


;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ;
;; MISC stuff
;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ;

(defn sleep-ms
  "Puts the current thread to sleep for number of milliseconds provided"
  [sleep-ms]
  (try (Thread/sleep (to-long sleep-ms))
       (catch InterruptedException ie)))

(defn get-fast-ms
  "Returns milliseconds since some arbitrary point in time"
  []
  (long (/ (System/nanoTime) 1000000)))


;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ;
;; JSON stuff
;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ;

(defonce json-object-mapper
  (json/object-mapper {:decode-key-fn std-keyword
                       :encode-key-fn name}))

(defn read-json [s]
  (json/read-value s json-object-mapper))

(defn- write-json [edn]
  (json/write-value-as-string edn json-object-mapper))


;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ;
;; Handle keyboard events
;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ;

(defonce event-queue
  (java.util.concurrent.LinkedBlockingQueue. event-queue-size))

(defn push-event [ev]
  (.put event-queue ev))

(defn get-lazy-queue [^java.util.concurrent.LinkedBlockingQueue q]
  "Return a lazy sequence of objects from the queue provided.
   calls 'take' so removes elements from the queue, and waits if nothing is available"
  (repeatedly #(.take q)))

(defn get-key-text [kc]
  (com.github.kwhat.jnativehook.keyboard.NativeKeyEvent/getKeyText kc))

;; As pulled from;
;; https://github.com/kwhat/jnativehook/blob/2.2/src/main/java/com/github/kwhat/jnativehook/keyboard/NativeKeyEvent.java
;;
;; NOTE: characters that require a holding SHIFT aren't yet supported. Ex '!' which is SHIFT+1
(def keycode
  {30 \a
   48 \b
   46 \c
   32 \d
   18 \e
   33 \f
   34 \g
   35 \h
   23 \i
   36 \j
   37 \k
   38 \l
   50 \m
   49 \n
   24 \o
   25 \p
   16 \q
   19 \r
   31 \s
   20 \t
   22 \u
   47 \v
   17 \w
   45 \x
   21 \y
   44 \z
   ;; 11 \0
   ;; 2 \1
   ;; 3 \2
   ;; 4 \3
   ;; 5 \4
   ;; 6 \5
   ;; 7 \6
   ;; 8 \7
   ;; 9 \8
   ;; 10 \9
   ;; 28 \newline
   57 \space
   12 \-
   40 \'
   14 \backspace
   28 \return
   })

(def modifier-keycodes
  {42   :shift
   54   :shift
   29   :control
   3613 :control
   56   :alt
   3640 :alt
   })

(def tokenizing-characters
  "A set of characters used to split words"
  #{nil ;; An unrecognized key
    \space
    \return
    \newline
    })

;; Define what should happen when OS gets key press and release events
;;
(defonce native-key-listener
  (reify com.github.kwhat.jnativehook.keyboard.NativeKeyListener
    (nativeKeyPressed [this ev]
      (if @logging-on
        (push-event [ev :pressed (get-fast-ms)])))
    (nativeKeyReleased [this ev]
      (if @logging-on
        (push-event [ev :released (get-fast-ms)])))
    (nativeKeyTyped [this ev]
      (do)) ;; Ignore this event
    ))


;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ;
;; Handle words
;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ;

(defonce word-queue
  (java.util.concurrent.LinkedBlockingQueue. word-queue-size))

(defn push-word [ev]
  (.put word-queue ev))

(defn take-word []
  (.take word-queue))

(defn process-events []
  (some->> event-queue
           get-lazy-queue
           (reduce (fn [[agg mods ats] [k e ts]]
                     (let [kc (.getKeyCode k)
                           key-char (keycode kc)
                           reset-state nil
                           delta (- ts ats) ;; Length of time between events
                           ]
                       (if (= e :pressed)

                         ;; Handle backspace
                         (if (and (= \backspace key-char)
                                  (not (empty? agg)))
                           [(subvec agg 0 (dec (count agg))) mods ts]

                           ;; Ignore key press if either ALT or CTRL are held down
                           (if-not (or (mods :alt)
                                       (mods :control)
                                       (= \backspace key-char) ;; Ignore backspace on an empty buffer as well
                                       )

                            ;; Case it's a mod key
                            (if-let [mod (modifier-keycodes kc)]
                              [agg (conj mods mod) ts]

                              ;; Case it's a key press of a non-Mod key
                              (let [new-key [key-char delta]]

                                ;; Determine if we should push a word or not
                                (if (or (> delta 3000)
                                        (contains? tokenizing-characters key-char))

                                  ;; Word termination: Push word and clear agg
                                  (do (if (not (empty? agg))
                                        (push-word agg))
                                      (if (contains? tokenizing-characters key-char) ;; Don't add tokenizing characters to buffer
                                        [[] mods ts]
                                        [[new-key] mods ts]))

                                  ;; Still in a word: keep aggregating
                                  [(conj agg new-key) mods ts])))

                            ;; Ignoring the key press, as it's ALT or CTRL, or "empty" backspace
                            [agg mods ts]))

                         ;; Case it's key release
                         [agg
                          (if-let [mod (modifier-keycodes kc)]
                            (disj mods mod) ;; Remove mod, as key released
                            mods)
                          ats])))

                   [[] #{} 0])))


;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ;
;; Seesaw Utilities
;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ;

(defmacro my-invoke-later [& body]
  `(seesaw.invoke/invoke-later ~@body nil))

(defmacro my-invoke-now [& body]
  `(seesaw.invoke/invoke-now ~@body nil))

(defn ta-print
  "Prints screen to primary text area"
  [string]
  (if @textarea-link
    (.appendText @textarea-link (str string "\n"))
    (println (str "TA: " string "\n"))))

(defn ta-log
  "Prints a log entry, Time stamp prefix, to the text area"
  [string]
  (let [time-stamp (.format (java.text.SimpleDateFormat. "HH:mm:ss")
                            (java.util.Date.))]
    (ta-print (str time-stamp " - " string))))

(defn create-button
  "Creates a button with an event associated with it"
  [text fn-arg]
  (doto (java.awt.Button. text)
    (.addActionListener (proxy [java.awt.event.ActionListener] []
                          (actionPerformed [e]
                            (try (fn-arg)
                                 (catch Exception e (ta-log (str text " Failed: " e)))))))))


;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ;
;; Handle data indexing
;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ;

(defn add-word-to-freq-table
  [^String s]
  (when (okay-string? s)
    (swap! word-freq-table assoc s (inc (get @word-freq-table s 0)))))

(defn debug-word-output
  []
  (loop [word (take-word)]
      (println (str "RAW: '" word "'"))
      (if @debug-on
        (recur (take-word)))))

(defn debug-to-index
  []
  (thread
    (loop [word-data (take-word)]
      (let [word (apply str (keep first word-data))
            status (if (some->> word-data
                                (filter first) ;; Ignore key's that aren't handled
                                (map second)
                                ;; rest ;; Ignore first letter as it's the start of the word
                                (every? #(< % 10)))
                         " CHORD"
                         "")
            ]
        (if (okay-string? word)
          (do
            (my-invoke-later
             (ta-log (str "Word: " word status)))
            (add-word-to-freq-table (str word status))))
        (recur (take-word))))))


;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ;
;; Seesaw Utilities
;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ;

(defmacro my-invoke-later [& body]
  `(seesaw.invoke/invoke-later ~@body nil))

(defmacro my-invoke-now [& body]
  `(seesaw.invoke/invoke-now ~@body nil))


;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ;
;; MAIN stuff
;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ;

;; Defined further down
(declare cc-chord-table)

(defn display-report
  []
  (let [report-frame (java.awt.Frame. "Word Frequency Report")
        close-button (create-button "Close"
                                    #(.dispose report-frame))
        table-data (or (some->> word-freq-table
                                deref
                                (sort-by second >)
                                (keep (fn [[w f]]
                                        (if (> f 2)
                                          {:token w
                                           :freq f
                                           :chord (get cc-chord-table w "")
                                           })))
                                vec)
                       []) ;; Return an empty vec
        report-table (doto (ss/table :model [:columns [{:key :token  :text "Word"}
                                                       {:key :freq   :text "Frequency"}
                                                       {:key :chord  :text "Chord"}]
                                             :rows table-data]
                                     ;; :id :report-table
                                     ;; :show-grid? true
                                     )
                       (.setAutoCreateRowSorter true))
        main-panel (mig/mig-panel :constraints ["fill, ins 0"]
                                  :items [[(ss/scrollable report-table) "grow"]
                                          [close-button "dock south"]])
        ;; main-panel (doto (java.awt.Panel.)
        ;;              (.add "Center" (ss/scrollable report-table))
        ;;              (.add "South" close-button))
        ]

    ;; Display reports
    (my-invoke-later
     (doto report-frame
       (.add "Center" main-panel)
       .pack
       .show))))

(defn save-index-to-disk
  "Write word index to disk"
  [filename]
  (some->> word-freq-table
           deref
           write-json
           (spit filename)))

(defn load-index-from-disk
  "Load an existing word index from disk"
  [filename]
  (try (some->> filename
                slurp
                (json/read-value) ;; Don't use read-json, because we don't want JSON key's to be Clojure keywords
                (reset! word-freq-table))
       (catch Exception e
         (ta-log (str "Failed to load log: " e))))
  (if (nil? @word-freq-table)
    (reset! word-freq-table {})))

(defn shutdown-handler
  "Save state and shut the application down"
  []
  (try
    (save-index-to-disk default-index-filename)
    (com.github.kwhat.jnativehook.GlobalScreen/removeNativeKeyListener native-key-listener)
    (com.github.kwhat.jnativehook.GlobalScreen/unregisterNativeHook)
    (System/exit 0)

    (catch Exception _
      ;; Not much to do, we're shutting down...
      )))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (do
    (ss/native!)
    (let [frame (java.awt.Frame. "CharaChorder Helper")
          textarea (java.awt.TextArea.)
          main-panel (doto (java.awt.Panel.)
                       (.add "Center" textarea))
          quit-button (create-button "Quit"
                                     #(do
                                        (.dispose frame) ;; Close the window
                                        (shutdown-handler)))
          start-button (create-button "Start Logging"
                                      #(do (reset! logging-on (System/currentTimeMillis))
                                           (ta-log "Starting logging")))
          report-button (create-button "Report"
                                       display-report)
          stop-button (create-button "Stop Logging"
                                     #(do (let [start-ts @logging-on]
                                            (reset! logging-on nil)
                                            (if start-ts
                                              (ta-log (str "Stop logging after " (long (/ (- (System/currentTimeMillis) start-ts) (* 60 1000))) "m"))
                                              (ta-log "Stop logging")))))
          top-panel (doto (java.awt.Panel.)
                      (.add start-button)
                      (.add report-button)
                      (.add stop-button))
          sub-panel (doto (java.awt.Panel.)
                      (.add quit-button))

          ;; Kick off background threads to interpret key events
          event-thread (thread (process-events))
          word-thread (thread (debug-to-index)) ;; Use this for now
          ]

      ;; Register listeners
      (try
        (com.github.kwhat.jnativehook.GlobalScreen/registerNativeHook)
        (com.github.kwhat.jnativehook.GlobalScreen/addNativeKeyListener native-key-listener)
        (catch Exception e
          (ta-log (str "Failed to register listeners: " e))))


      ;; Cleanly shutdown if the X in the title bar is clicked
      (ss/listen (ss/to-root frame)
                 :window-closing (fn [_] (shutdown-handler)))

      ;; Bring up UI
      (my-invoke-later
       (doto frame
         (.add "North" top-panel)
         (.add "Center" main-panel)
         (.add "South" sub-panel)
         .pack
         .show))

      ;; Init UI fields in case of errors...
      (reset! textarea-link textarea)

      ;; Load existing index from disk if it's available
      (load-index-from-disk default-index-filename)

      ;; end -main
      )))



;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ;
;; CharaChorder default chord table
;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ;

(def cc-chord-table
  {"about" "about"
   "above" "above"
   "add" "ad"
   "after" "after"
   "again" "agin + dup"
   "air" "air"
   "all" "al"
   "almost" "almost"
   "along" "along"
   "also" "also"
   "always" "alwys"
   "an" "an"
   "and" "and"
   "animal" "animl"
   "another" "another"
   "answer" "answer"
   "any" "any"
   "are" "are"
   "around" "around"
   "as" "as"
   "ask" "ask"
   "at" "at"
   "away" "awy + dup"
   "back" "back"
   "be" "be"
   "because" "because"
   "been" "ben"
   "before" "befor"
   "began" "began"
   "begin" "begin"
   "being" "be + present"
   "below" "below"
   "between" "betwn"
   "big" "big"
   "book" "bok"
   "both" "bot"
   "boy" "boy"
   "but" "but"
   "by" "by"
   "call" "cal"
   "came" "came"
   "can" "can"
   "car" "car"
   "carry" "cary"
   "change" "chan"
   "children" "children"
   "city" "city"
   "close" "close"
   "come" "come"
   "could" "could"
   "country" "country"
   "cut" "cut"
   "day" "day"
   "did" "di"
   "different" "difernt"
   "do" "do"
   "does" "does"
   "don't" "don't"
   "down" "down"
   "each" "each"
   "earth" "earth"
   "eat" "eat"
   "end" "end"
   "enough" "enough"
   "even" "evn"
   "every" "evry + dup"
   "example" "exampl"
   "eye" "ey + dup"
   "face" "face"
   "family" "family"
   "far" "far"
   "father" "father"
   "feet" "fet"
   "few" "few"
   "find" "find"
   "first" "first"
   "follow" "folw"
   "food" "fod"
   "for" "for"
   "form" "frm"
   "found" "found"
   "four" "four"
   "from" "from"
   "get" "get"
   "girl" "girl"
   "give" "gie"
   "go" "go"
   "good" "go + dup"
   "got" "got"
   "great" "great"
   "group" "group"
   "grow" "grow"
   "had" "had"
   "hand" "hand"
   "hard" "hard"
   "has" "has"
   "have" "have"
   "he" "he"
   "head" "head"
   "hear" "hear"
   "hello" "helo + dup"
   "help" "help"
   "her" "her"
   "here" "her + dup"
   "high" "hig"
   "him" "him"
   "his" "his"
   "home" "home"
   "house" "house"
   "how" "how"
   "idea" "idea"
   "if" "if"
   "important" "imprtn"
   "in" "in"
   "into" "into"
   "is" "is"
   "it" "it"
   "it's" "it's"
   "its" "it + plural"
   "just" "just"
   "keep" "kep"
   "kind" "kind"
   "know" "know"
   "land" "land"
   "large" "large"
   "last" "last"
   "later" "later"
   "learn" "learn"
   "leave" "leav"
   "left" "left"
   "let" "let"
   "letter" "letr"
   "life" "life"
   "light" "light"
   "like" "like"
   "line" "line"
   "list" "list"
   "little" "lite"
   "live" "live"
   "long" "long"
   "look" "lok"
   "made" "made"
   "make" "make"
   "man" "man"
   "many" "many"
   "may" "may"
   "me" "me"
   "mean" "mean"
   "men" "men"
   "might" "might"
   "mile" "mile"
   "miss" "mis"
   "more" "more"
   "most" "most"
   "mother" "mother"
   "mountain" "mountain"
   "move" "move"
   "much" "much"
   "must" "must"
   "my" "my"
   "name" "nae"
   "near" "near"
   "need" "ne + dup"
   "never" "nevr"
   "new" "new"
   "next" "next"
   "night" "nigh"
   "no" "n + dup"
   "not" "not"
   "now" "now"
   "number" "number"
   "of" "of"
   "off" "o + dup"
   "often" "often"
   "oil" "oil"
   "old" "old"
   "on" "on"
   "once" "once"
   "one" "one"
   "only" "only"
   "open" "open"
   "or" "or"
   "other" "other"
   "our" "our"
   "out" "out"
   "over" "over"
   "own" "ow"
   "page" "page"
   "paper" "paer"
   "part" "part"
   "people" "peol"
   "picture" "picture"
   "place" "place"
   "plant" "plant"
   "play" "play"
   "point" "point"
   "put" "put"
   "question" "question"
   "quick" "quick"
   "quickly" "quickly"
   "quite" "quite"
   "read" "read"
   "really" "realy"
   "right" "right"
   "river" "rive"
   "run" "run"
   "said" "said"
   "same" "same"
   "saw" "saw"
   "say" "say"
   "school" "schol"
   "sea" "sea"
   "second" "second"
   "see" "se + dup"
   "seem" "sem + dup"
   "sentence" "sentc"
   "set" "set"
   "she" "she"
   "should" "should"
   "show" "show"
   "side" "side"
   "small" "smal + dup"
   "so" "so"
   "some" "some"
   "something" "something"
   "sometimes" "somet + plural"
   "song" "song"
   "soon" "son + dup"
   "sound" "sound"
   "spell" "spel"
   "start" "star + dup"
   "state" "state"
   "still" "stil + dup"
   "stop" "stop"
   "story" "story"
   "study" "stud"
   "such" "such"
   "take" "take"
   "talk" "talk"
   "tell" "tel + dup"
   "than" "than"
   "that" "tha"
   "the" "te"
   "their" "their"
   "them" "them"
   "then" "then"
   "there" "ther"
   "these" "thes"
   "they" "they"
   "thing" "thing"
   "think" "think"
   "this" "this"
   "those" "those"
   "thought" "thoug"
   "three" "thre + dup"
   "through" "throug"
   "time" "time"
   "to" "to"
   "together" "togehr"
   "too" "to + dup"
   "took" "tok"
   "tree" "tre + dup"
   "try" "try"
   "turn" "turn"
   "two" "two"
   "under" "under"
   "until" "until"
   "up" "up"
   "us" "us"
   "use" "use"
   "very" "very"
   "walk" "walk"
   "want" "want"
   "was" "wa"
   "watch" "wath"
   "water" "water"
   "way" "way"
   "we" "we"
   "well" "wel"
   "went" "went"
   "were" "wer + dup"
   "what" "wha"
   "when" "when"
   "where" "wher"
   "which" "whi + dup"
   "while" "while"
   "white" "white"
   "who" "wh"
   "why" "why"
   "will" "wil + dup"
   "with" "with"
   "without" "without"
   "word" "word"
   "work" "work"
   "world" "world"
   "would" "would"
   "write" "writ"
   "year" "yea"
   "you" "you"
   "young" "young"
   "your" "your"})





;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ;
;; Comments
;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ; ;; ;

(comment
;; START COMMENTS START COMMENTS START COMMENTS START COMMENTS

  ;;
  ;; Notification support
  ;;

  ;; works on mac and windows

  ;; Check to see if support is available
  (java.awt.SystemTray/isSupported)

  ;; https://github.com/mbsmith/clj-systemtray/blob/master/src/clj_systemtray/core.clj

  (def mitem-foo (java.awt.MenuItem. "foo"))
  (def mitem-bar (java.awt.CheckboxMenuItem. "bar"))
  (def popup-menu (doto (java.awt.PopupMenu.)
                    (.add mitem-foo)
                    (.add mitem-bar)
                    ))

  (def stray (java.awt.SystemTray/getSystemTray))
  (def image (.createImage (java.awt.Toolkit/getDefaultToolkit) "icon.png"))
  (def tray-icon (doto (java.awt.TrayIcon. image "Tray Demo")
                   (.setImageAutoSize true)
                   (.setToolTip "System tray icon demo")
                   (.setPopupMenu popup-menu) ;; to display the popup menu and appear in stray
                   ))
  (.add stray tray-icon)

  ;; display message works, but system-tray doesn't seem to add icon on mac

  ;; Displays the message!!!
  (.displayMessage tray-icon "Hello World" "Notification Demo" java.awt.TrayIcon$MessageType/INFO)




  ;;
  ;; Key Logging support
  ;;
  (com.github.kwhat.jnativehook.GlobalScreen/registerNativeHook)
  (com.github.kwhat.jnativehook.GlobalScreen/addNativeKeyListener native-key-listener)
  (com.github.kwhat.jnativehook.GlobalScreen/removeNativeKeyListener native-key-listener)
  (com.github.kwhat.jnativehook.GlobalScreen/unregisterNativeHook)


  (def p-menu (popup-menu
               (menu-item "foo" (fn [] (println "foo")))
               (separator)
               (menu-item "bar" (fn [] (println "bar")))
               ))



  ;; END COMMENTS END COMMENTS END COMMENTS END COMMENTS
  )
