(ns facts+
  (:use midje.sweet
        [midje.production-mode :only [user-desires-checking?]]
        [clojure.walk :only [prewalk prewalk-replace postwalk]]))

(defmulti provided-magic
  "Use defprovided/defnprovided instead. Dispatches on symbol converted from
   string/keyword/symbol."
  #(-> % name symbol))

(defmacro defnprovided
  "Creates a new `magic` keyword for provided+ forms. Use this for provided+
   forms with any size of input"
  [key args & func]
  (let [names (vec (map name args))]
    `(defmethod provided-magic '~key
       [x#]
       (fn [~args] (prewalk-replace
                   (apply hash-map
                          (interleave (map symbol ~names)
                                      ~args))
                   '~func)))))

(defmacro defprovided
  "Creates a new `magic` keyword for provided+ forms. Use this for provided+
   forms for only one input."
  [key & forms]
  `(defmethod provided-magic '~key
     [x#]
     (fn [v#] (prewalk-replace {'~key v#} '~forms))))

;; TODO create macro for generating multiple tests easily

(defprovided nocall (nocall & anything) => irrelevant :times 0)

(defprovided ignore (ignore & anything) => irrelevant :times truthy)

(defnprovided called [f num] (f & anything) => irrelevant :times num)

(defnprovided always [f value] (f & anything) => value)

(defn form-type
  "return a keyword based on the type of the input form"
  [form]
  (cond
   (keyword? form) :metadata
   (string? form) :description
   (seq? form) (case (first form)
                 provided :provided
                 provided+ :provided+
                 :misc)
   ;; TODO handle these: they could be metadata
   ;; https://github.com/marick/Midje/wiki/Metadata#defining-metadata
   :else (throw (IllegalArgumentException. (str "form-type else: " form)))))

(defn gather-tests
  "groups forms that are tests (based on the position of the => arrow), and
   forms that are not tests"
  [forms]
  (let [forms (vec forms)
        indices (filter identity (map-indexed (fn [i v]
                                                (when (= '=> v) i))
                                              forms))
        surrounding (mapcat (juxt dec identity inc) indices)
        surrounding-set (set surrounding)
        grouped (group-by #(contains? surrounding-set %) (range (count forms)))
        test-indices (grouped true)
        other-indices (grouped false)
        test-forms (map #(nth forms %) test-indices)
        other-forms (map #(nth forms %) other-indices)]
    ;; makes sure there is no overlap
    (assert (= (count surrounding) (count surrounding-set)))
    [test-forms other-forms]))

(defn facts+provided
  "merges provided and provided+ values into one provided block"
  [p p+]
  (let [ps (mapcat rest p)
        p+s (mapcat rest p+)
        transformed (mapcat (fn [[k v]] ((provided-magic k) v))
                            (partition 2 p+s))]
  (concat ps transformed)))

(defn facts+description
  "creates a description for multiple descriptions"
  [prev-description descriptions]
  (let [descriptions (concat (when prev-description [prev-description])
                             descriptions)]
    (when (seq descriptions)
      (list (apply str (interpose " - " descriptions))))))

(defn facts+helper
  "See facts+. Performs the features of facts+ on a group of forms."
  ([forms] (facts+helper forms nil [] []))
  ([forms prev-description prev-provided prev-metadata]
     (let [[tests others] (gather-tests forms)
           {:keys [description metadata provided provided+ misc]}
           (group-by form-type others)
           new-metadata (concat prev-metadata metadata)
           new-description (facts+description prev-description description)
           new-provided (concat prev-provided
                                (facts+provided provided provided+))
           new-tests (mapcat #(concat % [`(provided ~@new-provided)])
                             (partition 3 tests))
           ;; TODO recurse into misc to work for forms like let, around, etc.
           new-misc (mapcat #(facts+helper (rest %)
                                            (first new-description)
                                            new-provided
                                            new-metadata)
                             misc)]
       (concat [`(fact ~@new-description ~@new-metadata ~@new-tests)]
               new-misc))))

(defmacro facts+
  "Midje's facts, but with modifications, namely:
   -merging provided clauses
   -propagating metadata
   -propagating provided clauses
   -allowing for magic provided statements

   Works with:
   -midje-mode

   Still unconfirmed:
   -tabular
   -against-background
   -fact-group

   Caveats:
   -tests under the same fact must share provided forms
   -let bindings and any other inner forms will not have metadata from inner
    facts propagate outward properly. this is because in order to get to an
    inner fact, all previous outer facts must pass the filter first. this is
    normally handled by separating the inner fact into a new fact, but this
    can't be done with for example a let form because it would change the
    meaning of the code:
      Normal Example:
        (fact :a (fact :b 1 => 1)) ;; filter wont work for :b
      With facts+:
        (facts+ :a (fact :b 1 => 1))
      is converted to:
        (fact :a :b 1 => 1)
      This can't be done (with my current knowledge of midje)  with let et al
      because it may cause recomputing expensive / side effect generating code.

   Current limitations:
   -facts+ doesn't recurse down all forms (e.g. let blocks)
   -facts+ only supports keyword metadata
   -facts within tests won't be propagated to. for example:
    (do 1 => 1 1) => 1

   Assumptions:
   -tests only consist of 3 forms, with the => symbol in the middle
   -metadata is only in the form of keywords
   -only fact, facts, facts+, provided, and provided+ are handled specially"
  [& forms]
  (when (user-desires-checking?)
    (let [new-forms (facts+helper forms)]
      `(do ~@(doall (map #(with-meta % (meta &form)) new-forms))))))

(facts+ :facts+ :provided-magic
  "About provided-magic"

 (fact "About nocall"
   ((provided-magic :nocall) 'free)
   => '((free & anything) => irrelevant :times 0))

 (fact "About ignore"
   ((provided-magic :ignore) 'free)
   => '((free & anything) => irrelevant :times truthy)

   (fact
     1 => 1
     (provided-magic :aaaa) => irrelevant
     (provided+ :ignore provided-magic)))

 (fact "About called"
   ((provided-magic :called) ['free 4])
   => '((free & anything) => irrelevant :times 4)

   (fact
     (list (provided-magic) (provided-magic)) => irrelevant
     (provided-magic (provided-magic)) => irrelevant
     (provided-magic (provided-magic 3)) => irrelevant
     (provided+ :called [provided-magic 2])))

 (fact "About always"
   ((provided-magic :always) ['free 42])
   => '((free & anything) => 42)

   (fact
     (provided-magic) => 42
     (provided-magic 1) => 42
     (provided-magic nil 3 (range)) => 42
     (provided+ :always [provided-magic 42])))
 )

;; ----------
;; Demonstrating facts+
;; ----------

;; facts+ will share/propagate provided forms
(facts+
 :facts+
 (facts+helper 1) => 1
 (facts+helper 1) => 1
 (provided (facts+helper 1) => 1))
;; fact/facts will not
(facts
 :facts+
 (facts+helper 1) => 1
 (provided (facts+helper 1) => 1)
 (facts+helper 1) => 1
 (provided (facts+helper 1) => 1))

;; facts+ can inherit in a tree-like pattern to create more complex tests /
;; minimize repetition
(facts+
 :facts+
 (facts
   (+ (facts+helper 1) (facts+helper 2)) => 3
   (provided (facts+helper 2) => 2))
 (facts
   (+ (facts+helper 1) (facts+helper 2)) => 4
   (provided (facts+helper 2) => 3))
 (provided (facts+helper 1) => 1))

;; prevent repeating repetitive provided statements as well
(facts+
 :facts+
 1 => 1
 (provided+ :nocall facts+helper))

;; write new shortcuts for your own use cases
(defprovided odd-calls (odd-calls & anything) => irrelevant :times odd?)

(facts+ :facts+
 "About odd-calls"
 ((provided-magic :odd-calls) 'free)
 => '((free & anything) => irrelevant :times odd?))

(facts+
 :facts+
 (facts+helper 3) => irrelevant ; this causes the provided form to fail
 (provided+ :odd-calls facts+helper))

;; propagate metadata
(facts+
 :facts+
 (fact (fact (fact 1 => 1)))) ;; gets called when filtering for facts+
(facts
 :facts+
 (fact 1 => 1)) ;; doesn't get called when filtering for facts+

;; don't ignore extra docstrings
(facts+
 :facts+
 "1"
 (facts "2"
   (facts "3"
     1 => 1) ;; docstring is "1 - 2 - 99 - 3"
   "99"))
(facts :facts+
 "1"
 (facts "2" :facts+
   (facts "3" :facts+
     1 => 1) ;; docstring is "1 - 2 - 3"
   "99"))


;; Below this is the new implementation

(defn seq-to-map
  "Converts a sequence of pairs of vectors to a map"
  [s]
  (->> s (apply concat) (apply hash-map)))

(facts "About seq-to-map"
  (seq-to-map '([3 4] [1 [2]])) => {3 4 1 [2]})

(defn to-walker
  "Converts a function that is meant to be applied on sequences to one that
   can be applied to most of clojure's basic data structures."
  [func]
  (fn [form]
    (cond
     (seq? form) (func form)
     :else form)))

(tabular
 (facts "About to-walker"
   (let [double #(concat % %)
         add #(conj % :a)]
     ((to-walker double) in) => out1
     ((to-walker add) in) => out2))
 in out1 out2
 :3 :3 :3
 'a 'a 'a
 1.5 1.5 1.5
 [3 4] [3 4] [3 4]
 '(a b) '(a b a b) '(:a a b)
 {:a :b} {:a :b} {:a :b})

(tabular
 (facts "About to-walker w/ postwalk"
   (let [double #(concat % %)
         add #(conj % :a)]
     (postwalk (to-walker double) in) => out1
     (postwalk (to-walker add) in) => out2
     ))
 in out1 out2
 :3 :3 :3
 '(a) '(a a) '(:a a)
 '(a (b)) '(a (b b) a (b b)) '(:a a (:a b)))

(defn facts+test?
  "Whether or not the input is a valid representation of a test."
  [form]
  (if (not (map? form)) false
      (let [{:keys [test provided]} form]
        (and (or (seq? test) (vector? test))
             (or (seq? provided) (vector? provided))
             (-> form meta ::facts+ (= :test))))))

(tabular
 (facts "About facts+test?"
   (facts+test? in) => out)
 in out
 '() falsey
 {:a :b} falsey
 {:provided [] :test []} falsey
 {:provided [] :metadata {} :data []} falsey
 (with-meta {:provided [] :metadata {} :data []} {::facts+ :fact}) falsey
 (with-meta {:provided [] :metadata {} :data []} {::facts+ :test}) falsey
 (with-meta {:provided [] :test []} {::facts+ :fact}) falsey
 (with-meta {:provided [] :test []} {::facts+ :test}) truthy
 (with-meta {:provided [] :test {}} {::facts+ :test}) falsey
 (with-meta {:provided {} :test []} {::facts+ :test}) falsey)

(defn facts+fact?
  "Whether or not the input is a valid representation of a fact."
  [form]
  (if (not (map? form)) false
      (let [{:keys [metadata provided data]} form]
        (and (or (seq? provided) (vector? provided))
             (or (seq? data) (vector? data))
             (map? metadata)
             (-> form meta ::facts+ (= :fact))))))

(tabular
 (facts "About facts+fact?"
   (facts+fact? in) => out)
 in out
  '() falsey
 {:a :b} falsey
 {:provided [] :test []} falsey
 {:provided [] :metadata {} :data []} falsey
 (with-meta {:provided [] :metadata {} :data []} {::facts+ :fact}) truthy
 (with-meta {:provided [] :metadata {} :data []} {::facts+ :test}) falsey
 (with-meta {:provided [] :test []} {::facts+ :fact}) falsey
 (with-meta {:provided [] :test []} {::facts+ :test}) falsey
 (with-meta {:provided [] :metadata {} :data {}} {::facts+ :fact}) falsey
 (with-meta {:provided {} :metadata {} :data []} {::facts+ :fact}) falsey
 (with-meta {:provided [] :metadata [] :data []} {::facts+ :fact}) falsey
 '((f x) => 3) falsey)

(defn facts+transform-tests
  "Scan through form and replaces tests with easily parsable format"
  [forms]
  (if (#{'provided+ 'provided* 'provided} (first forms))
    forms
    (loop [[f1 f2 f3 & rst :as forms] forms
           done []]
      (cond
       (empty? forms) (apply list done) ; return a list as well
       (= f2 '=>) (let [t [f1 f2 f3]
                        next-form (first rst)
                        provided? (and (seq? next-form)
                                       (= (first next-form) 'provided))
                        p (if provided? (rest next-form) [])
                        new-forms (if provided? (rest rst) rst)
                        test (with-meta {:test t :provided p} {::facts+ :test})]
                    (recur new-forms (conj done test)))
       :else (recur (rest forms) (conj done f1))))))

(tabular
 (facts "About facts+transform-tests"
   (facts+transform-tests forms) => done)
 forms done
 '() '()
 '(x y) '(x y)
 '(x => y) '({:test (x => y) :provided ()})
 '(a x => y (provided a) b) '(a {:test (x => y) :provided (a)} b)
 '(x => y 3) '({:test (x => y) :provided ()} 3)
 '(x => y) #(= :test (::facts+ (meta (first %))))
 '(provided 1 => 2) '(provided 1 => 2))

(defn facts+merge-meta
  "Used for merging midje metadata that doesn't overwrite old data."
  [metadata new-metadata]
  (let [merged (merge metadata new-metadata)
        name1 (:midje/name metadata)
        name2 (:midje/name new-metadata)
        desc1 (:midje/description metadata)
        desc2 (:midje/description new-metadata)
        merge-name (when (and name1 name2) (str name1 "-" name2))
        merge-desc (when (and desc1 desc2) (str desc1 " - " desc2))]
    (merge metadata
           new-metadata
           (when merge-name {:midje/name merge-name})
           (when merge-desc {:midje/description merge-desc}))))

(tabular
 (facts "About facts+merge-meta"
   (facts+merge-meta m1 m2) => m3)
 m1 m2 m3
 {} {:a 1} {:a 1}
 {:b 2} {} {:b 2}
 {:a :b :b 3} {:a :c} {:a :c :b 3}
 {:a :b} nil {:a :b}
 nil {:c :d} {:c :d}
 {:midje/name "a"} {} {:midje/name "a"}
 {} {:midje/name "b"} {:midje/name "b"}
 {:midje/name "c"} {:midje/name "d"} {:midje/name "c-d"}
 {:midje/description "a"} {} {:midje/description "a"}
 {} {:midje/description "b"} {:midje/description "b"}
 {:midje/description "c"} {:midje/description "d"} {:midje/description "c - d"})

(defn facts+gather-meta
  "Scan through forms and combine metadata"
  [forms]
  (loop [[fst & rst :as forms] (seq forms)
         metadata {}
         done []]
    (cond
     (empty? forms) [metadata done]
     ;; this shouldn't be called, because tests should have been transformed
     (= '=> (second forms)) (recur (drop 3 forms)
                                   metadata
                                   (concat done (take 3 forms)))
     (symbol? fst) (recur (conj rst {:midje/name (name fst)}) metadata done)
     (keyword? fst) (recur (conj rst {fst true}) metadata done)
     (string? fst) (recur (conj rst {:midje/description fst}) metadata done)
     (map? fst) (if (::facts+ (meta fst))
                  (recur rst metadata (concat done [fst]))
                  (recur rst (facts+merge-meta metadata fst) done))
     :else (recur rst metadata (concat done [fst])))))

(tabular
 (facts "About facts+gather-meta"
   (facts+gather-meta forms) => [metadata done])
 forms metadata done
 '() {} '()
 '(:a) {:a true} '()
 '({3 4}) {3 4} '()
 '({1 2} {1 3}) {1 3} '()
 '(3) {} '(3)
 '(b) {:midje/name "b"} '()
 '(b 1.5 c) {:midje/name "b-c"} '(1.5)
 '(b 1.5 c => d) {:midje/name "b"} '(1.5 c => d)
 '("a") {:midje/description "a"} '()
 '("a" b e => "f" "c") {:midje/description "a - c" :midje/name "b"} '(e => "f")
 '({1 2} {1 3} => {1 4}) {1 2} '({1 3} => {1 4})
 '(b (prn x) c) {:midje/name "b-c"} '((prn x))
 [{:a 1}] {:a 1} []
 [(with-meta {:a 1} {::facts+ true})] {} [{:a 1}])

(defn facts+provided+
  "Transforms provided* forms"
  [forms]
  (mapcat (fn [[k v]] ((provided-magic k) v)) (partition 2 forms)))

(facts "About facts+provided+"
  (facts+provided+ `(:nocall foo :nocall bar))
  => (contains ['=> :times 0 '=> :times 0] :gaps-ok))

(declare provided+)

(declare provided*)

(defn facts+gather-provided
  "Scan through forms and combine/transform provided forms"
  [forms]
  (loop [[h & t :as forms] forms
         provided []
         done []]
    (if (empty? forms) [provided done]
        (if (not (seq? h)) (recur t provided (conj done h))
            (let [f (first h)]
              (cond
               (= f 'provided*) (recur t (concat provided (rest h)) done)
               (= f 'provided+) (recur t (concat provided
                                                 (facts+provided+ (rest h)))
                                       done)
               :else (recur t provided (conj done h))))))))

(tabular
 (facts "About facts+gather-provided"
   (facts+gather-provided forms) => [prov done])
 forms prov done
 '(3 4 5) '() '(3 4 5)
 '((provided* 3 4)) '(3 4) '()
 '((provided* 1) (provided* 2)) '(1 2) '()
 '((provided* 1) 2 (provided* 3)) '(1 3) '(2))

(tabular
 (facts "About facts+gather-provided w/ provided+"
   (facts+gather-provided forms) => [prov done]
   (provided
     (provided-magic & anything) => (fn [x] [:hi])))
 forms prov done
 '((provided+ :nocall a) (provided* c)) '(:hi c) '())

(defn facts+transform-fact
  "Transforms a sequence of forms in a fact into a parsable format"
  [forms]
  {:pre [(seq? forms)]}
  (let [transformed (facts+transform-tests forms)
        [metadata no-meta] (facts+gather-meta transformed)
        [prov remaining] (facts+gather-provided no-meta)]
    (with-meta
      {:metadata metadata :provided prov :data remaining}
      {::facts+ :fact})))

(facts "About facts+transform-fact"
  (facts+transform-fact '(:a :b :c {:d "hi"} c a => b (provided c)))
  =>  {:metadata {:midje/name "c", :d "hi", :c true, :b true, :a true},
       :provided [],
       :data [{:test '(a => b)
               :provided '(c)}]}
  (facts+transform-fact '(:a 'b "c")) => #(= :fact (::facts+ (meta %)))
  (facts+transform-fact 3) => (throws AssertionError)
  (facts+transform-fact '(:a)) => {:metadata {:a true} :provided [] :data []}
  (facts+transform-fact '()) => {:metadata {} :provided [] :data []})

(defn facts+transform-facts
  "Transforms fact forms and returns other forms unchanged"
  [[h & t :as forms]]
  (if (#{'fact 'facts 'facts+} h)
    (facts+transform-fact (apply list t))
    forms))

(tabular
 (facts "About facts+transform-facts"
   (facts+transform-facts forms) => result)
 forms result
 '(:a) '(:a)
 '(fact) {:metadata {} :provided [] :data []}
 '(facts+ :a (prn x)) {:metadata {:a true} :provided [] :data ['(prn x)]}
 '(fact) #(= :fact (::facts+ (meta %)))
 '(:a) #(not= :fact (::facts+ (meta %))))

(defn facts+propagate-to-tests
  "Adds metadata/provided parts of a fact to a test if the input is a test
   TODO test"
  [{:keys [metadata provided] :as fact} form]
  (if (not (facts+test? form)) form
      (let [test (:test form)
            test-prov (:provided form)
            test-metadata (:metadata form)]
        (with-meta
          {:metadata (facts+merge-meta metadata test-metadata)
           :test test
           :provided (concat test-prov provided)}
          (meta form)))))

(defn facts+propagate-inward
  "Propagates metadata/provided fact of a fact to all inner tests, if the
   input is a fact
   TODO test"
  [form]
  (if (not (facts+fact? form)) form
      (with-meta
        (assoc form :data (postwalk #(facts+propagate-to-tests form %)
                                    (:data form)))
        (meta form))))

(defn i [x]
  (prn)
  (prn x)
  (prn)
  x)

(defn facts+reverse-merge-meta
  "Merges in reverse (to prefer keeping fields in older metadata)"
  [old-meta new-meta]
  (merge (dissoc new-meta :midje/name :midje/description) old-meta))

(defn facts+reduce-metadata
  "Returns outwardly merged metadata of all facts underneath the form."
  [form]
  (let [acc (atom {})
        add-meta #(swap! acc facts+reverse-merge-meta (:metadata %))]
    ;; Using prewalk order so that there is a consistent left to right
    ;; pattern of merging
    (prewalk (fn [x] (when (facts+fact? x) (add-meta x)) x) form)
    @acc))

(defn facts+propagate-outward
  "Propagates metadata of all inner tests to a fact, if the input is a fact"
  [form]
  (if (not (facts+fact? form)) form
      (with-meta
        (assoc form :metadata (facts+reduce-metadata form))
        (meta form))))

(defn facts+recreate-facts
  "Recreates the normal clojure form of facts."
  [form]
  (if (not (facts+fact? form)) form
      (let [{:keys [metadata data]} form]
        (concat (list `fact metadata) data))))

(defn facts+recreate-tests
  "Recreates the normal clojure form of tests"
  [form]
  (if (not (facts+test? form)) form
      (let [{:keys [metadata test provided]} form]
        (concat (list `fact metadata)
                test
                [(concat (list `provided) provided)]))))

(defn facts+transform
  [forms]
  (->> (concat ['facts+] forms)
       ;; replace tests with easily parsable data structures
       (postwalk (to-walker facts+transform-tests))
       ;; replace facts and perform magic on new provided statements
       (postwalk (to-walker facts+transform-facts))
       ;; propagate metadata / provided statements inward to all tests
       (postwalk facts+propagate-inward)
       ;; propagate metadata outward to enclosing facts
       (postwalk facts+propagate-outward)
       ;; recreate facts
       (postwalk facts+recreate-facts)
       ;; recreate tests as facts (so that they can be filtered)
       (postwalk facts+recreate-tests)))

(defmacro facts+2
  "

  CAVEATS:
  -all tests will be converted to facts (this is so that tests can be filtered
   by metadata, while the metadata of the containing facts can be changed to
   allow for deeper searches"
  [& forms]
  (facts+transform forms))

(defn f [] 1)

(facts+2
 :a
 (f) => 3
 (fact :b (f) => 3
   (fact :c (f) => 3))
 (provided* (f) => 3)
 (let [x 3]
   (f) => 3
   (f)) => 3)

;; TODO delete unused functions


;; facts+ will share/propagate provided forms
(facts+2
 :facts+
 (facts+helper 1) => 1
 (facts+helper 1) => 1
 (provided* (facts+helper 1) => 1))

;; facts+ can inherit in a tree-like pattern to create more complex tests /
;; minimize repetition
(facts+2
 :facts+
 (facts
   (+ (facts+helper 1) (facts+helper 2)) => 3
   (provided (facts+helper 2) => 2))
 (facts
   (+ (facts+helper 1) (facts+helper 2)) => 4
   (provided (facts+helper 2) => 3))
 (provided* (facts+helper 1) => 1))

;; prevent repeating repetitive provided statements as well
(facts+2
 :facts+
 1 => 1
 (provided+ :nocall facts+helper))

;; write new shortcuts for your own use cases
(defprovided odd-calls (odd-calls & anything) => irrelevant :times odd?)

;; NOTE: this call fails with facts+
(facts :facts+
 "About odd-calls"
 ((provided-magic :odd-calls) 'free)
 => '((free & anything) => irrelevant :times odd?))

(facts+2
 :facts+
 (facts+helper 3) => irrelevant ; this causes the provided form to fail
 (provided+ :odd-calls facts+helper))

;; propagate metadata
(facts+2
 :facts+
 (fact (fact (fact 1 => 1)))) ;; gets called when filtering for facts+
(facts
 :facts+
 (fact 1 => 1)) ;; doesn't get called when filtering for facts+

;; don't ignore extra docstrings
(facts+2
 :facts+
 "1"
 (facts "2"
   (facts "3"
     1 => 1) ;; docstring is "1 - 2 - 99 - 3"
   "99"))
(facts :facts+
 "1"
 (facts "2" :facts+
   (facts "3" :facts+
     1 => 1) ;; docstring is "1 - 2 - 3"
   "99"))
