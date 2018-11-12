# stoicos-reason

## Description
Stoicos interpretor in reasonml.
Made to be able to use the language in a browser using js and react.
Also to experiment some coding style and design.

## Usage
Donwload this repo then open src/Index.html
Write your code in the textarea and press the interpret button. Any output will
apear below. Use the clear button to clear the output.

You can also use [this page](jaimelesjeux.fr/Stoicos/) but this may not be up to date.

Some examples :
```
#this is a simple hello world
print "hello world"
```

```
#same with a variable
= :text "hello world"
print text
```

```
#same with a function
= :hello (fun [:x] {(print "hello" x)})
hello "world"
```

```
#prints 4 factorial
= :fact (fun [:x]
    {
      (if (<= x 0)
        {1}
        {(* x (fact (- x 1)))}
      )
    }
  )
print (fact 4)
```
