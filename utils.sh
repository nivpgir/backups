

backup_if_exists(){
    # if the file itself exists, and there is no backup file,
    # create a backup file and exit
    if [ -e $1 ]; then
        if [ ! -e $1.bak ]; then
            local backup_file=$1.bak
            echo backing up $1 to $backup_file
        else
            # else, while filename.bak.$i exists, increment i, then create filename.bak.$i
            i=1
            while [ -e $1.bak.$i ]; do
                echo $1.bak.$i exists...
                ((i++))
            done
            local backup_file=$1.bak.$i
            echo backing up $1 to $backup_file
        fi
        mv $1 $backup_file
    fi
    # if file doesn't exist, we have nothing to do
}



# this exists for educational purposes:
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
