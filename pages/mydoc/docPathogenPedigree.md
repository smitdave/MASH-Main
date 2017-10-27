---
title: Pathogen Pedigrees
sidebar: mydoc_sidebar
permalink: docPathogenPedigree.html
folder: mydoc
toc: true
summary: "Description of pathogen pedigree tracking."
---
# Plasmodium falciparum (PfPedigree)

The pedigree functions are necessary to track how pathogens move between hosts and mosquito vectors. Pedigree tracking must occur when a new human infection occurs; that is, when sporozites successfully invade the host bloodstream. In programmatic terms, this means that the function <code>addPf2Pedigree()</code> will be called during the evaluation of <code>infectiousBite_XX()</code>.

## PfPedigree Generics

To initialize the pedigree tracking routines please note that, following convention we will refer to the specific instantiation of PfPedigree as PfPedigree_XX, where XX will be that specific instantiation. However there are certain generic steps that must be undertaken, even if one is using the NULL instantiation.

1. ffsa
2. fs
3. fs
4. 

## PfPedigree_full







{% highlight r %}
addEvent2Q(ixH, event){
  ...
}
{% endhighlight %}
