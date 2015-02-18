(ns active.lawrence.direct-lr-test
  (:require [active.lawrence.grammar-test :refer :all]
            [active.lawrence.grammar :refer :all]
            [active.lawrence.runtime :refer :all]
            [active.lawrence.direct-lr :as direct]
            [clojure.test :refer :all]))

(defn input
  "Convert list of [token attribute-value] vectors to input."
  [g vs]
  (map (fn [[t av]]
         (make-pair (grammar-symbol->name t g)
                    av))
       vs))

(defn parse
  [g m vs]
  (direct/parse g 1 m (input g vs)))

(defn should-accept
  [g vs]
  (is (= :accept (parse g :lr vs)))
  (is (= :accept (parse g :slr vs))))

(defn should-error
  [g vs]
  (is (thrown? Exception (direct/parse g 1 :lr (input g vs))))
  (is (thrown? Exception (direct/parse g 1 :slr (input g vs)))))

(deftest g00-test
  (should-accept g00 (list [:l 5]))
  (should-error g00 (list))
  (should-error g00 (list [:l 5] [:l 7])))

(deftest g08-test
  (should-accept g08 (list [:l 5] [:r 5]))
  (should-accept g08 (list [:l 5] [:l 5] [:r 5] [:r 5]))
  (should-error g08 (list))
  (should-error g08 (list [:l 5] [:l 5] [:l 5] [:r 5]))
  (should-error g08 (list [:l 5] [:l 7])))

(deftest g10-test
  (should-accept g10 (list [:l nil] [:n 5] [:+ nil] [:n 7] [:r nil] [:* nil] [:n 10]))
  (should-error g10 (list [:l nil] [:n 5] [:+ nil] [:n 7] [:* nil] [:n 10])))
                                                                             
  


