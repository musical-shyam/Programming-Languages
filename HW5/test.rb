def foo(x)
    y = 2
    bar = proc { 
        print x, "\n"
        y = 3
    }
    bar.call()
    print y, "\n"
end

foo(3)