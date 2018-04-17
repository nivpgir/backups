
function myfunc()
{
    # this is a trick to pass the variable to assign to a function
    # what happens here is that if a second argument is supplied,
    # it is treated as a variable name to be set, and the return value
    # will be availabe through that variable, otherwise, the return
    # value is echoed.
    local  __resultvar=$2
    local  myresult=$1
    if [[ "$__resultvar" ]]; then
        eval $__resultvar="'$myresult'"
    else
        echo "$myresult"
    fi
}

# bar is passed as the variable name
myfunc foo bar
# so bar is available and holds the value, foo
echo $bar

# no variable was given, foo is echoed
result2=$(myfunc foo)
echo $result2
