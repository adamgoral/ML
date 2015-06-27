MathJax.Hub.Config({
    skipStartupTypeset: true,
    messageStyle: "none",
    "HTML-CSS": {
        showMathMenu: false
    }
});
MathJax.Hub.Configured();

var myApp = angular.module("math-editor", []);

myApp.directive("mathjaxBind", function () {
    return {
        restrict: "A",
        controller: ["$scope", "$element", "$attrs", function ($scope, $element, $attrs) {
            $scope.$watch($attrs.mathjaxBind, function (value) {
                var $script = angular.element("<script type='math/tex'>")
                    .html(value == undefined ? "" : value);
                $element.html("");
                $element.append($script);
                MathJax.Hub.Queue(["Reprocess", MathJax.Hub, $element[0]]);
            });
        }]
    };
});

function MyCtrl($scope, $element) {
    $scope.expression = "";
}
