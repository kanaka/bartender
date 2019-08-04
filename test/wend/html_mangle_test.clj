(ns wend.html-mangle-test
  (:require [clojure.test :refer :all]
            [wend.html-mangle :as html-mangle]))

(def test1-attrs "
<html> <body>
  <div  class  =  \"bar\"    class   =   \"baz\"  >
    World
  </div>
</body> </html>")

(def test1-attrs-first "
<html> <body>
  <div  class  =  \"bar\"  >
    World
  </div>
</body> </html>")

(def test1-attrs-merge "
<html> <body>
  <div  class  =  \"bar baz\"  >
    World
  </div>
</body> </html>")

;; TODO: this could be split to test each prune capability
(def test2 "
<html><head>
  <meta name=\"foo\" property=\"bar\" content=\"baz\">
  <link rel=\"stylesheet\" href=\"./basics.css\">
  <meta name=\"bar\" content=\"baz\">
  <style type=\"text/css\">
    div {
      width: 600px;
    }
  </style>
</head>
<body>
<div>  <div id=\"div1\" style=\"border-radius: 5px;\">
        div content
        <span class=\"myspan\" class=\"another-class\"
              style=\"background-color: #f55;\"  style=\"color: #384858\">span content</span>
</div>  </div>

<svg xmlns:xlink=\".../xlink\">
<defs>
  <symbol id=\"icon-home\">
  </symbol>
</defs>
</svg>
  <span id=\"xyz\" for=\"div1\">foo</span>
  <select required=\"required\"></select>
<svg xmlns:xlink=\".../xlink\">
<defs>
</defs>
</svg>
  <script  type=\"text/javascript\" src=\"foo.js\"></script>
<!-- commment should stay unindented -->

<script  type=\"text/javascript\">
  console.log('hello');
</script>

xp&#x00c9;

</body>
</html>")

(def test2-extract-html "<html><head><meta name=\"bar\" content=\"baz\"></head><body><div><div id=\"div1\">div content<span class=\"myspan\">span content</span></div></div><span id=\"xyz\">foo</span><select required=\"true\"></select><!--commment should stay unindented-->xp&#x00c9;</body></html>")

(def test2-extract-inline-css
  "border-radius: 5px;\nbackground-color: #f55;\ncolor: #384858")

(def test3 "  <html><head><meta name=\"foo\" property=\"bar\" content=\"baz\"><link rel=\"stylesheet\" href=\"./basics.css\"><meta name=\"bar\" content=\"baz\"></head><body>  <div class=\"mydiv\"\n  style=\"border-radius: 5px;\"  >content   <div>  </div> </div></body>   </html>")

(def test3-indented "<html>
  <head>
    <meta name=\"foo\" property=\"bar\" content=\"baz\">
    <link rel=\"stylesheet\" href=\"./basics.css\">
    <meta name=\"bar\" content=\"baz\">
  </head>
  <body>
    <div class=\"mydiv\" style=\"border-radius: 5px;\">
      content
      <div>
      </div>
    </div>
  </body>
</html>
")

(deftest html-mangle-attrs-test
  (testing "TnA->html after TnA-parse should be identical"
    (is (= test1-attrs
           (-> test1-attrs
               html-mangle/TnA-parse
               html-mangle/TnA->html)))
    (is (= test2
           (-> test2
               html-mangle/TnA-parse
               html-mangle/TnA->html)))
    (is (= test3
           (-> test3
               html-mangle/TnA-parse
               html-mangle/TnA->html))))
  (testing "use first val for dupe attrs"
    (is (= test1-attrs-first
           (-> test1-attrs
               html-mangle/TnA-parse
               (html-mangle/TnA->html {:dupe-attr-mode :first})))))
  (testing "merge dupe attrs vals"
    (is (= test1-attrs-merge
           (-> test1-attrs
               html-mangle/TnA-parse
               (html-mangle/TnA->html {:dupe-attr-mode :merge}))))))


(deftest html-mangle-extract-html-test
  (testing "extract-html tags/attribute pruning"
    (is (= test2-extract-html
           (html-mangle/extract-html test2)))))

(deftest html-mangle-extract-inline-css-test
  (testing "extract-inline-css"
    (is (= test2-extract-inline-css
           (html-mangle/extract-inline-css test2)))))

(deftest html-mangle-reindenting
  (testing "testing reindenting of HTML"
    (is (= test3-indented
           (-> test3
               html-mangle/TnA-parse
               (html-mangle/TnA->html {:reindent? true}))))))


