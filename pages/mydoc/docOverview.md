---
title: Documentation
sidebar: mydoc_sidebar
permalink: docOverview.html
folder: mydoc
toc: false
summary: "MASH is being developed with modularity as one of its primary goals. As such most of the components can be plugged in and out as required for a specific application. The documentation presented here shows a brief description of most of the modules found on the main repository."
---

## Steps to clone documentation branch

The first step to be able to update and make changes to the documentation is to clone its gh-pages following these steps:

1. Open the terminal
2. Move to the folder you want the branch to be cloned
3. Input the following command:

{% highlight bash %}
git clone -b gh-pages https://github.com/smitdave/MASH-Development.git
{% endhighlight %}


## Steps to update documentation

The documentation site is based on markdown (a brief tutorial can be found  <a href="https://guides.github.com/features/mastering-markdown/">here</a>) and it can use HTML tags and syntax.

Most of the time, the only thing needed will be to modify existing pages. To do it, follow the next steps:

1. Modify the desired .md file in pages/mydoc/
2. Make sure it follows the MD and/or HTML syntax
3. Commit your changes to the repository

## Steps to add a bew documentation page

In case a new page needs to be added follow the next steps:

1. Add page to: pages/mydoc/
2. Update: title, permalink (make it match the filename)
3. Update: data/sidevars/mydoc_sidebar (if needed)

## Steps to run the jekyll server locally

Running the jekyll server locally is possible, although difficult. The problem is that some of the Ruby gems tend to collide with each other and this causes all sorts of problems. A thorough description of how to install the required libraries will be created in the future.
