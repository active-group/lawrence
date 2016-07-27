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
         (make-pair (grammar-name->symbol t g)
                    av))
       vs))

(defn parse
  [g m vs]
  (direct/parse g 1 m (input g vs)))

(defn should-accept
  [g vs res]
  (is (= res (parse g :lr vs)))
  (is (= res (parse g :slr vs))))

(defn should-error
  [g vs]
  (is (thrown? Exception (direct/parse g 1 :lr (input g vs))))
  (is (thrown? Exception (direct/parse g 1 :slr (input g vs)))))

(deftest g00-test
  (should-accept g00 (list [:l]) nil)
  (should-error g00 (list))
  (should-error g00 (list [:l] [:l])))

(deftest g08-test
  (should-accept g08 (list [:l] [:r]) nil)
  (should-accept g08 (list [:l] [:l] [:r] [:r]) nil)
  (should-error g08 (list))
  (should-error g08 (list [:l] [:l] [:l] [:r]))
  (should-error g08 (list [:l] [:l])))

(deftest g10-test
  (should-accept g10 (list [:l] [:n 5] [:+] [:n 7] [:r] [:*] [:n 10]) 120)
  (should-error g10 (list [:l] [:n 5] [:+] [:n 7] [:*] [:n 10])))

(deftest g10-error-test
  (should-accept g10-error (list [:l] [:n 5] [:+] [:n 7] [:r] [:*] [:n 10]) 120)
  (should-accept g10-error (list [:l] [:n 5] [:+] [:+] [:n 7] [:r] [:*] [:n 10]) 0)
  (should-accept g10-error (list [:l] [:n 5] [:+] [:+] [:n 7] [:r] [:+] [:n 10]) 10))


(deftest goptional-test
  ;;(should-accept goptional (list [:bar]) :absent)
  (should-accept goptional (list [:bar] [:foo]) :present))

