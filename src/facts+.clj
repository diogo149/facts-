(ns facts+
  (:use midje.sweet
        [midje.production-mode :only [user-desires-checking?]]
        [clojure.walk :only [prewalk prewalk-replace postwalk]]))

;; TODO rename facts+... functions now that they have their own namespace

(declare facts+)

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

(defn test?
  "Whether or not the input is a valid representation of a test."
  [form]
  (if (not (map? form)) false
      (let [{:keys [test provided]} form]
        (and (or (seq? test) (vector? test))
             (or (seq? provided) (vector? provided))
             (-> form meta ::facts+ (= :test))))))

(tabular
 (facts "About test?"
   (test? in) => out)
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

(defn fact?
  "Whether or not the input is a valid representation of a fact."
  [form]
  (if (not (map? form)) false
      (let [{:keys [metadata provided data]} form]
        (and (or (seq? provided) (vector? provided))
             (or (seq? data) (vector? data))
             (map? metadata)
             (-> form meta ::facts+ (= :fact))))))

(tabular
 (facts "About fact?"
   (fact? in) => out)
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

(defn transform-tests
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
 (facts "About transform-tests"
   (transform-tests forms) => done)
 forms done
 '() '()
 '(x y) '(x y)
 '(x => y) '({:test (x => y) :provided ()})
 '(a x => y (provided a) b) '(a {:test (x => y) :provided (a)} b)
 '(x => y 3) '({:test (x => y) :provided ()} 3)
 '(x => y) #(= :test (::facts+ (meta (first %))))
 '(provided 1 => 2) '(provided 1 => 2))

(defn merge-meta
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
 (facts "About merge-meta"
   (merge-meta m1 m2) => m3)
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

(defn gather-meta
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
                  (recur rst (merge-meta metadata fst) done))
     :else (recur rst metadata (concat done [fst])))))

(tabular
 (facts "About gather-meta"
   (gather-meta forms) => [metadata done])
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

(defn transform-provided+
  "Transforms provided+ forms"
  [forms]
  (mapcat (fn [[k v]] ((provided-magic k) v)) (partition 2 forms)))

(facts "About transform-provided+"
  (transform-provided+ `(:nocall foo :nocall bar))
  => (contains ['=> :times 0 '=> :times 0] :gaps-ok))

(defn gather-provided
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
                                                 (transform-provided+ (rest h)))
                                       done)
               :else (recur t provided (conj done h))))))))

(tabular
 (facts "About gather-provided"
   (gather-provided forms) => [prov done])
 forms prov done
 '(3 4 5) '() '(3 4 5)
 '((provided* 3 4)) '(3 4) '()
 '((provided* 1) (provided* 2)) '(1 2) '()
 '((provided* 1) 2 (provided* 3)) '(1 3) '(2))

(tabular
 (facts "About gather-provided w/ provided+"
   (gather-provided forms) => [prov done]
   (provided
     (provided-magic & anything) => (fn [x] [:hi])))
 forms prov done
 '((provided+ :nocall a) (provided* c)) '(:hi c) '())

(defn transform-fact
  "Transforms a sequence of forms in a fact into a parsable format"
  [forms]
  {:pre [(seq? forms)]}
  (let [transformed (transform-tests forms)
        [metadata no-meta] (gather-meta transformed)
        [prov remaining] (gather-provided no-meta)]
    (with-meta
      {:metadata metadata :provided prov :data remaining}
      {::facts+ :fact})))

(facts "About transform-fact"
  (transform-fact '(:a :b :c {:d "hi"} c a => b (provided c)))
  =>  {:metadata {:midje/name "c", :d "hi", :c true, :b true, :a true},
       :provided [],
       :data [{:test '(a => b)
               :provided '(c)}]}
  (transform-fact '(:a 'b "c")) => #(= :fact (::facts+ (meta %)))
  (transform-fact 3) => (throws AssertionError)
  (transform-fact '(:a)) => {:metadata {:a true} :provided [] :data []}
  (transform-fact '()) => {:metadata {} :provided [] :data []})

(defn transform-facts
  "Transforms fact forms and returns other forms unchanged"
  [[h & t :as forms]]
  (if (#{'fact 'facts 'facts+} h)
    (transform-fact (apply list t))
    forms))

(tabular
 (facts "About transform-facts"
   (transform-facts forms) => result)
 forms result
 '(:a) '(:a)
 '(fact) {:metadata {} :provided [] :data []}
 '(facts+ :a (prn x)) {:metadata {:a true} :provided [] :data ['(prn x)]}
 '(fact) #(= :fact (::facts+ (meta %)))
 '(:a) #(not= :fact (::facts+ (meta %))))

(defn propagate-to-tests
  "Adds metadata/provided parts of a fact to a test if the input is a test
   TODO test"
  [{:keys [metadata provided] :as fact} form]
  (if (not (test? form)) form
      (let [test (:test form)
            test-prov (:provided form)
            test-metadata (:metadata form)]
        (with-meta
          {:metadata (merge-meta metadata test-metadata)
           :test test
           :provided (concat test-prov provided)}
          (meta form)))))

(defn propagate-inward
  "Propagates metadata/provided fact of a fact to all inner tests, if the
   input is a fact"
  [form]
  (if (not (fact? form)) form
      (with-meta
        (assoc form :data (postwalk #(propagate-to-tests form %)
                                    (:data form)))
        (meta form))))

(defn reverse-merge-meta
  "Merges in reverse (to prefer keeping fields in older metadata)"
  [old-meta new-meta]
  (merge (dissoc new-meta :midje/name :midje/description) old-meta))

(defn reduce-metadata
  "Returns outwardly merged metadata of all facts underneath the form."
  [form]
  ;; FIXME using an atom and a walk as a hacky substitue for a stateful walk
  (let [acc (atom (:metadata form))
        add-meta #(swap! acc reverse-merge-meta (:metadata %))]
    ;; Using prewalk order so that there is a consistent left to right
    ;; pattern of merging
    (prewalk (fn [x] (when (test? x) (add-meta x)) x) form)
    @acc))

(defn propagate-outward
  "Propagates metadata of all inner tests to a fact, if the input is a fact"
  [form]
  (if (not (fact? form)) form
      (with-meta
        (assoc form :metadata (reduce-metadata form))
        (meta form))))

(defn recreate-facts
  "Recreates the normal clojure form of facts."
  [form]
  (if (not (fact? form)) form
      (let [{:keys [metadata data]} form]
        (concat (list `fact metadata) data))))

(defn recreate-tests
  "Recreates the normal clojure form of tests"
  [form]
  (if (not (test? form)) form
      (let [{:keys [metadata test provided]} form]
        (concat (list `fact metadata)
                test
                [(concat (list `provided) provided)]))))

(defn facts+transform
  [forms]
  (->> (concat ['facts+] forms)
       ;; replace tests with easily parsable data structures
       (postwalk (to-walker transform-tests))
       ;; replace facts and perform magic on new provided statements
       (postwalk (to-walker transform-facts))
       ;; TODO recreate facts in quoted forms
       ;; TODO recreate tests in quoted forms
       ;; propagate metadata / provided statements inward to all tests
       (postwalk propagate-inward)
       ;; propagate metadata outward to enclosing facts
       (postwalk propagate-outward)
       ;; recreate facts
       (postwalk recreate-facts)
       ;; recreate tests as facts (so that they can be filtered)
       (postwalk recreate-tests)))

(defmacro facts+
  "Midje's facts, but with modifications centered around a hierarchical
   structure of facts, namely:
   -propagating metadata (both inward and outward) to minimize repetition
    and manually handling propagation
   -sharing/inheriting provided* forms
   -allowing for magic provided+ statements

   Still untested:
   -tabular
   -against-background
   -fact-group
   -possibly other midje features...

   Caveats:
   -Quoted tests/forms will be converted (this can be easily solved)

   Assumptions:
   -tests only consist of 3 forms, with the => symbol in the middle
   -only fact, facts, facts+, provided, provided+ and provided* are
    handled specially"
  [& forms]
  (facts+transform forms))

;; ----------
;; Demonstrating facts+
;; ----------

(declare f)

;; facts+ will share/propagate provided forms
(facts+
 :facts+
 (f 1) => 1
 (f 1) => 1
 (provided* (f 1) => 1))
;; fact/facts will not
(facts
 :facts+
 (f 1) => 1
 (provided (f 1) => 1)
 (f 1) => 1
 (provided (f 1) => 1))

;; facts+ can inherit in a tree-like pattern to create more complex tests /
;; minimize repetition
(facts+
 :facts+
 (facts
   (+ (f 1) (f 2)) => 3
   (provided (f 2) => 2))
 (facts
   (+ (f 1) (f 2)) => 4
   (provided (f 2) => 3))
 (provided* (f 1) => 1))

(facts+
 :a
 (f) => 3
 (fact :b (f) => 3
   (fact :c (f) => 3))
 (provided* (f) => 3)
 (let [x 3]
   (f) => 3
   (f)) => 3)

;; prevent repeating repetitive provided statements as well
(facts+
 :facts+
 1 => 1
 (provided+ :nocall f))

;; write new shortcuts for your own use cases
(defprovided odd-calls (odd-calls & anything) => irrelevant :times odd?)

;; NOTE: this call fails with facts+
(facts :facts+
 "About odd-calls"
 ((provided-magic :odd-calls) 'free)
 => '((free & anything) => irrelevant :times odd?))

(facts+
 :facts+
 (f 3) => irrelevant ; this causes the provided form to fail
 (provided+ :odd-calls f))

(defnprovided three [f num] (f num) => 3)

(facts+
 :facts+
 (f 42) => 3
 (provided+ :three [f 42]))

;; propagate metadata
(facts+
 :facts+
 (fact (fact (fact 1 => 1)))) ;; gets called when filtering for facts+
(facts
 :facts+
 (fact 1 => 1)) ;; doesn't get called when filtering for facts+

(facts+ :a
  (facts+ :b 1 => 1)) ;; gets called when filtering for :a, :b, or both

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

(facts :facts+ :provided-magic
  "About provided-magic"
  (fact "About nocall"
    ((provided-magic :nocall) 'free)
    => '((free & anything) => irrelevant :times 0))

  (fact "About ignore"
    ((provided-magic :ignore) 'free)
    => '((free & anything) => irrelevant :times truthy))

  (fact "About called"
    ((provided-magic :called) ['free 4])
    => '((free & anything) => irrelevant :times 4))

  (fact "About always"
    ((provided-magic :always) ['free 42])
    => '((free & anything) => 42))
  )

(facts+ :facts+ :provided-magic
  "About provided-magic"

 (fact "About ignore"
   1 => 1
   (provided-magic :aaaa) => irrelevant
   (provided+ :ignore provided-magic))

 (fact "About called"
   (list (provided-magic) (provided-magic)) => irrelevant
   (provided-magic (provided-magic)) => irrelevant
   (provided-magic (provided-magic 3)) => irrelevant
   (provided+ :called [provided-magic 2]))

 (fact "About always"
   (provided-magic) => 42
   (provided-magic 1) => 42
   (provided-magic nil 3 (range)) => 42
   (provided+ :always [provided-magic 42]))
 )
