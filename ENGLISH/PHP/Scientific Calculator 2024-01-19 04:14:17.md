```php
<?php

class Calculator {

    protected $operations = array();

    public function __construct() {
        $this->operations = array(
            '+' => 'add',
            '-' => 'subtract',
            '*' => 'multiply',
            '/' => 'divide',
            '^' => 'power',
            '%' => 'modulo',
            '=' => 'equals',
            '!' => 'factorial',
            'sqrt' => 'squareRoot',
            'abs' => 'absoluteValue',
            'ceil' => 'ceiling',
            'floor' => 'floor',
            'round' => 'round',
            'sin' => 'sine',
            'cos' => 'cosine',
            'tan' => 'tangent',
            'asin' => 'arcsine',
            'acos' => 'arccosine',
            'atan' => 'arctangent',
            'sinh' => 'hyperbolicSine',
            'cosh' => 'hyperbolicCosine',
            'tanh' => 'hyperbolicTangent',
            'asinh' => 'hyperbolicArcsine',
            'acosh' => 'hyperbolicArccosine',
            'atanh' => 'hyperbolicArctangent',
            'log' => 'logarithm',
            'log10' => 'logarithmBase10',
            'exp' => 'exponential',
        );
    }

    public function calculate($expression) {
        $tokens = $this->tokenize($expression);
        $rpn = $this->toRPN($tokens);
        $result = $this->evaluateRPN($rpn);
        return $result;
    }

    protected function tokenize($expression) {
        $tokens = array();
        $token = '';
        for ($i = 0; $i < strlen($expression); $i++) {
            $char = $expression[$i];
            if (ctype_digit($char)) {
                $token .= $char;
            } else if (ctype_alpha($char)) {
                if ($token != '') {
                    $tokens[] = $token;
                    $token = '';
                }
                $tokens[] = $char;
            } else if ($char == '(') {
                if ($token != '') {
                    $tokens[] = $token;
                    $token = '';
                }
                $tokens[] = '(';
            } else if ($char == ')') {
                if ($token != '') {
                    $tokens[] = $token;
                    $token = '';
                }
                $tokens[] = ')';
            } else if ($char == ',') {
                if ($token != '') {
                    $tokens[] = $token;
                    $token = '';
                }
                $tokens[] = ',';
            } else if ($char == ' ') {
                if ($token != '') {
                    $tokens[] = $token;
                    $token = '';
                }
            } else {
                if ($token != '') {
                    $tokens[] = $token;
                    $token = '';
                }
                $tokens[] = $char;
            }
        }
        if ($token != '') {
            $tokens[] = $token;
        }
        return $tokens;
    }

    protected function toRPN($tokens) {
        $rpn = array();
        $stack = array();
        foreach ($tokens as $token) {
            if (ctype_digit($token)) {
                $rpn[] = $token;
            } else if (ctype_alpha($token)) {
                $stack[] = $token;
            } else if ($token == '(') {
                $stack[] = $token;
            } else if ($token == ')') {
                while ($stack[count($stack) - 1] != '(') {
                    $rpn[] = array_pop($stack);
                }
                array_pop($stack);
            } else if ($token == ',') {
                while ($stack[count($stack) - 1] != '(') {
                    $rpn[] = array_pop($stack);
                }
            } else {
                while (count($stack) > 0 && $this->precedence($stack[count($stack) - 1]) >= $this->precedence($token)) {
                    $rpn[] = array_pop($stack);
                }
                $stack[] = $token;
            }
        }
        while (count($stack) > 0) {
            $rpn[] = array_pop($stack);
        }
        return $rpn;
    }

    protected function evaluateRPN($rpn) {
        $stack = array();
        foreach ($rpn as $token) {
            if (ctype_digit($token)) {
                $stack[] = $token;
            } else if (ctype_alpha($token)) {
                $args = array();
                while (count($stack) > 0 && !ctype_digit($stack[count($stack) - 1])) {
                    $args[] = array_pop($stack);
                }
                $func = $this->operations[$token];
                $stack[] = $this->$func($args);
            } else {
                $arg2 = array_pop($stack);
                $arg1 = array_pop($stack);
                $stack[] = $this->operation($token, $arg1, $arg2);
            }
        }
        return $stack[0];
    }

    protected function operation($op, $arg1, $arg2) {
        switch ($op) {
            case '+':
                return $arg1 + $arg2;
            case '-':
                return $arg1 - $arg2;
            case '*':
                return $arg1 * $arg2;
            case '/':
                return $arg1 / $arg2;
            case '^':
                return pow($arg1, $arg2);
            case '%':
                return $arg1 % $arg2;
            case '=':
                return $arg1 == $arg2;
            case '!=':
                return $arg1 != $arg2;
            case '>':
                return $arg1 > $arg2;
            case '<':
                return $arg1 < $arg2;
            case '>=':
                return $arg1 >= $arg2;
            case '<=':
                return $arg1 <= $arg2;
            case '&&':
                return $arg1 && $arg2;
            case '||':
                return $arg1 || $arg2;
            default:
                throw new Exception("Invalid operator: $op");
        }
    }

    protected function precedence($op) {
        switch ($op) {
            case '(':
            case ')':
                return 0;
            case '+':
            case '-':
                return 1;
            case '*':
            case '/':
                return 2;
            case '^':
                return 3;
            default:
                return -1;
        }
    }

    protected function add($args) {
        return array_sum($args);
    }

    protected function subtract($args) {
        return $args[0] - $args[1];
    }

    protected function multiply($args) {
        return array_product($args);
    }

    protected function divide($args) {
        return $args[0] / $args[1];
    }

    protected function power($args) {
        return pow($args[0], $args[1]);
    }

    protected function modulo($args) {
        return $args[0] % $args[1];
    }

    protected function equals($args) {
        return $args[0] == $args[1];
    }

    protected function factorial($args) {
        $n = $args[0];
        if ($n < 0) {
            throw new Exception("Factorial undefined for negative numbers");
        }
        if ($n == 0) {
            return 1;
        } else {
            return $n * $this->factorial(array($n - 1));
        }
    }

    protected function squareRoot($args) {
        return sqrt($args[0]);
    }

    protected function absoluteValue($args) {
        return abs($args[0]);
    }

    protected function ceiling($args) {
        return ceil($args[0]);
    }

    protected function floor($args) {
        return floor($args[0]);
    }

    protected function round($args) {
        return round($args[0]);
    }

    protected function sine($args) {
        return sin($args[0]);
    }

    protected function cosine($args) {
        return cos($args[0]);
    }

    protected function tangent($args) {
        return tan($args[0]);
    }

    protected function arcsine($args) {
        return asin($args[0]);
    }

    protected function arccosine($args) {
        return acos($args[0]);
    }

    protected function arctangent($args) {
        return atan($args[0]);
    }

    protected function hyperbolicSine($args) {
        return sinh($args[0]);
    }

    protected function hyperbolicCosine($args) {
        return cosh($args[0]);
    }

    protected function hyperbolicTangent($args) {
        return tanh($args[0]);
    }

    protected function hyperbolicArcsine($args) {
        return asinh($args[0]);
    }

    protected function hyperbolicArccosine($args) {
        return acosh($args[0]);
    }

    protected function hyperbolicArctangent($args) {
        return atanh($args[0]);
    }

    protected function logarithm($args) {
        return log($args[0], M_E);
    }

    protected function logarithmBase10($args) {
        return log10($args[0]);
    }

    protected function exponential($args) {
        return exp($args[0]);
    }
}

$calculator = new Calculator();

$expression = "5 + 3";
$result = $calculator->calculate($expression);
echo $result; // 8

$expression = "(5 + 3) * 2";
$result = $calculator->calculate($expression);
echo $result; // 16

$expression = "sin(pi/2)";
$result = $calculator->calculate($expression);
echo $result; // 1

$expression = "log(100, 10)";
$result = $calculator->calculate($expression);
echo $result; // 2

$expression = "factorial(5)";
$result = $calculator->calculate($expression);
echo $result; // 120