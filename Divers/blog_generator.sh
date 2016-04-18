#!/bin/bash

# Initialize blog engine
if [ "$1" = "initialize" ]; then
    # create configuration file
    if [ ! -f "configuration" ]; then
        touch configuration
        echo "%author $USER" >> configuration
        echo "%email $USER@me.com" >> configuration
        echo "%articles articles" >> configuration
        mkdir -p articles
        mkdir -p templates
        touch templates/article.html
        touch templates/list.html
        touch templates/item.html
        echo "%articles-template templates/article.html" >> configuration
        echo "%articles-list templates/list.html" >> configuration
        echo "%articles-list-item templates/item.html" >> configuration
        # Create templates
        header="<!doctype html>\n<html>\n\t<head>\n"
        header+="\t\t<meta charset='utf-8'>\n"
        header+="\t</head>\n\t<body>\n\t\t"
        footer="\n\t</body>\n</html>"
        echo -e "$header<%title%><%content%>$footer" > templates/article.html
        echo -e "$header<ul>\n\t\t\t<%content%>\n\t\t</ul>$footer" > templates/list.html
        echo -e "<li><a href='<%url%>'><%title%></a></li>" > templates/item.html
        
    fi
fi 

# default values of configuration file
config='configuration'
raw_author='John Doe'
raw_email='jdoe@anonymous.com'
raw_title='Article without name'
sort_by='update'
repo='articles'
articles_template='templates/article.html'
list_template='templates/list.html'
items_template='templates/item.html'

# define current configuration
if [ $# -ge 1 ] && [ -f $1 ]; then
    config=$1
fi

# Extract data configuration
if [ -f $config ]
then
    # Attribution
    t=`sed -n '/^\%author /{p;q;}' $config`
    if [ -n "$t" ]; then
        raw_author=`echo "$t" |  sed -re 's/^\%author *//'`
    fi
    t=`sed -n '/^\%title /{p;q;}' $config`
    if [ -n "$t" ]; then
        raw_title=`echo "$t" |  sed -re 's/^\%title *//'`
    fi
    t=`sed -n '/^\%email /{p;q;}' $config`
    if [ -n "$t" ]; then
        raw_email=`echo "$t" |  sed -re 's/^\%email *//'`
    fi
    t=`sed -n '/^\%articles /{p;q;}' $config`
    if [ -n "$t" ]; then
        repo=`echo "$t" |  sed -re 's/^\%articles *//'`
    fi
    t=`sed -n '/^\%sort-type /{p;q;}' $config`
    if [ -n "$t" ]; then
        sort_by=`echo "$t" |  sed -re 's/^\%sort-type *//'`
    fi
    t=`sed -n '/^\%articles-template /{p;q;}' $config`
    if [ -n "$t" ]; then
        articles_template=`echo "$t" |  sed -re 's/^\%articles-template *//'`
    fi
    t=`sed -n '/^\%list-template /{p;q;}' $config`
    if [ -n "$t" ]; then
        list_template=`echo "$t" |  sed -re 's/^\%list-template *//'`
    fi
    t=`sed -n '/^\%items-template /{p;q;}' $config`
    if [ -n "$t" ]; then
        items_template=`echo "$t" |  sed -re 's/^\%items-template *//'`
    fi


fi

# Match ordonancement
if [ $sort_by = 'update' ]; then
    flag='-c'
elif [ $sort_by = 'rev-update' ]; then
    flag='-tr'
elif [ $sort_by = 'rev-name']; then
    flag='-r'
else
    flag=''
fi

# Iterate on each file
for file in `ls $flag --ignore="*~" $repo`
do
    # Default article's values
    basefile="${file%.*}"
    author="$raw_author"
    title="$raw_title"
    email="$raw_email
"
    # extract information of the article
    t=`sed -n '/^\%author /{p;q;}' $config`
    if [ -n "$t" ]; then
        author=`echo "$t" |  sed -re 's/^\%author *//'`
    fi
    t=`sed -n '/^\%title /{p;q;}' $config`
    if [ -n "$t" ]; then
        title=`echo "$t" |  sed -re 's/^\%title *//'`
    fi
    t=`sed -n '/^\%email /{p;q;}' $config`
    if [ -n "$t" ]; then
        email=`echo "$t" |  sed -re 's/^\%email *//'`
    fi

    # Fill List

    # Generate HTML file


    # A finir, trouver une manière propre de découper avec un "grand délimiteur"
    IFS="<%content%>" read after before < $articles_template 
    pandoc --webtex "articles/$file"  -o "$basefile.html"
    echo "$before"
    echo "$after"
    
done
