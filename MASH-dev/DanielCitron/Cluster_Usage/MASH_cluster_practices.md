# MASH IHME Cluster Use Practices

#### - Daniel Citron, 11/20/2017

<dl><hr></dl>

### Summary

1. Cluster Basics
2. Cluster Infrastructure - where to put stuff
3. Set up R on the cluster - use docker container version
4. Installing the MASH library in the right location
5. Workflow demonstrations
6. Additional tips and shortcuts

<dl><hr></dl>


### Before You Start
1. Obtain a cluster login
2. Take IHME's cluster orientation - slides can be found [here](https://hub.ihme.washington.edu/display/IHD/Cluster+Training+Home+Page)

<dl><hr></dl>

### IHME Cluster Basics

IHME has two clusters: one for development ("dev"), and one for production ("prod").
Use the dev cluster to test out your code and your scripts - it's less powerful
and less relied upon by other groups at IHME, so it's ok if it breaks.  The prod
cluster, on the other hand, is where IHME's powerful computation happens.  As a
result, you have to be a little more careful to not break things.  Every job that
you start on the prod cluster requires you to specify which of IHME's projects
the job belongs to and how much memory it requires.  For these reasons, make sure
that you've properly benchmarked your code before starting a large job on prod.

### Cluster Infrastructure

Here's how we're going to organize everything:

* __/ihme/malaria__ - this is our group's main directory.  All group members have access to this.  We will use this location to store data for simulation inputs as well as simulation results.  We will also use this location to store scripts for running the simulation that need to be shared across the group.
* __/ihme/malaria/[your name]__ - this is your directory within the group's main directory
* __/ihme/malaria/code)__ - this is where the most recent version of the R code will go. (Already, the MASH_0.1 library has been installed at this location for people to use.)
* __/homes/[your name]__ - this is your personal directory.  Use this location for testing, developing, benchmarking, or performing other tasks that do not need to be shared with the group.
* __/home/j/Project/malaria__ - this is on the J drive, which is backed up to tape and stored forever.  If you have some results or data that you believe should be stored forever, you can store it here.

We also need to be careful because not all directories are shared across dev and prod.
Our group directory at /ihme/malaria and our personal directories in /homes can be accessed from both dev and prod, but there are many path locations that are not shared in this way.  If you find that a file or directory mysteriously disappears when switching between dev and prod, this is why.

<dl><hr></dl>

### Logging in and getting started

When you ssh into either dev or prod, you land on a "login node." You'll be able
to navigate around the cluster and perform other ordinary command-line tasks, but
you will not be able to run any programs (such as R).  To begin an interactive
session on the cluster, (running R interactively, for example),
you will need to log in to one or more compute
nodes on the cluster. On dev, this is as straightforward as typing `qlogin` into
the command line.  On prod, things are a little more complicated.  A `qlogin` command
might look something like this:

`qlogin -N job_name -P proj_mmc -pe multi_slot 4 -l mem_free=8`

This command creates an interactive session (`qlogin`) named "job_name" (`-N`)
under the Malaria project (`-P proj_mmc`).  The session has requested a parallel
environment (`-pe multi_slot`) with 4 slots of 2 GB each (`-l mem_free` - each
slot yields 2 GB memory).
The Malaria Team's project name is `proj_mmc` - to start a job on prod you will
need to include `-P proj_mmc` as one of the options in your `qlogin`.

You can use an interactive session to run the code that you want, or you can use
the `qsub` command to submit jobs to the cluster.  The [cluster training module](https://stash.ihme.washington.edu/projects/TRAIN/repos/cluster_exercise/browse)
has a simple practice exercise that should help you get familiar with how this
works.  Open up a new branch under your username, clone the repository, and try
to get part 2B of the script to submit multiple `qsub` jobs to run in parallel.

If you want to check on the current jobs you have running on the cluster, use `qstat`.  If you want to delete a job, use `qdel [job-ID]`. (You may get an email reminding you to turn off a `qlogin` session or other job if it is left running for too long.)

<dl><hr></dl>

### Using R on the cluster

Once you've logged on to the cluster and used `qlogin`, if you run R from the
command line you will find that the default version of R is fairly outdated -
3.1.2 ("Pumpkin Helmet").
Instead of using the default, we will be accessing a more recent version of R.
Version 3.4.0 ("You Stupid Darkness") has been already installed for us inside a
docker container.  Add to your `.bash_profile` the following line:

`alias r_dock="/share/local/singularity/bin/singularity exec /share/singularity-images/rstudio/ihme_rstudio_3403.img /usr/local/bin/R"`

Now, when you execute `r_dock` in the command line, it will open the more recent
version of R that has already been configured for us in the docker container.

What else can we do with this?  The docker container's version of R is also
configured such that we can run RStudio sessions remotely on the cluster.  This
takes a little extra setup:
1. Add the following alias to your `.bash_profile`, replacing [4-digit port]
with your favorite 4-digit number - this will execute a shell
script that starts up an RStudio session on the cluster.

    `alias rstudio_ide="sh /share/singularity-images/rstudio/shells/rstudio_qsub_script.sh -p [4-digit port] -s 2 -P proj_mmc"`
2. Source your `.bash_profile`, and then run `rstudio_ide` in the command line
3. The script will output a bunch of text related to starting up the RStudio
session. (It does this by submitting a job to the cluster in the backend.), but
the last line will be a url that looks a little like this:

    `http://[qlogin node].ihme.washington.edu:[4-digit port]`
4. Go to that url in your browser to open the RStudio session.
5. NOTE: you will not be able to run multiple RStudio sessions at once - having
an RStudio session open on dev will prohibit opening an RStudio session on prod,
and vice-versa.
6. NOTE: All of this is dependent upon the docker container working correctly,
so if that goes wrong we'll need to get help Grant Nguyen.
7. Read more about the cluster's docker container build of R on IHME's hub
[here](https://hub.ihme.washington.edu/display/DKB/Launch+RStudio+IDE+and+Containerized+R+on+the+Cluster+using+Singularity)

<dl><hr></dl>

### Using Git on the cluster

We need to be careful when using Git to version control code that will be shared
between multiple people on the cluster.  For this reason, we want to avoid ever
having multiple users pulling MASH from GitHub to the same directory.  Instead,
all users who plan on having to edit and update the code will pull MASH into their
personal directory on /IHME.  For the users who are not actively developing MASH
and only want to use the most recent version, we will have a central shared location
on the cluster where all users can reference when they need to perform simulations.

As we move past the development stage and start using MASH for various applications, we will have a definitive version of MASH stored at __/ihme/malaria/code__.  At the same time, each user who is involved with development will create their own git repository in __/ihme/malaria/[your name]__ if they want to test a different version of the code.

To get started, `qlogin` and navigate to __/ihme/malaria/[your name]__ and do the following:

* `git init` - start your git repository
* `git clone https://github.com/smitdave/MASH-Main.git` - clone MASH

<dl><hr></dl>

### Installing the MASH Library

Now that we've created a link to the MASH git repository, we next have to build and install the MASH R package.  Navigate to __/ihme/malaria[your name]__ and do the following:

* Choose a location to install the MASH R package.  I created a directory called __/ihme/malaria/dtcitron/Rlibs__ to store all of my R packages
* First we need to install some dependencies: use `r_dock` or `rstudio_ide` to start an R session
  * `install.packages("deSolve", lib="/ihme/malaria/[your name]/Rlibs/")`
  * `install.packages("rootSolve", lib="/ihme/malaria/[your name]/Rlibs/")`
  * `install.packages("spatstat", lib="/ihme/malaria/[your name]/Rlibs/")`
* Now, exit the R session and build the MASH package from the command line:

`r_dock CMD INSTALL ---no-multiarch --with-keep.source --library=/ihme/malaria/[your name]/Rlibs /ihme/malaria/[your name]/MASH-Main/MASH-dev/DavidSmith/MASH_0.1/`
* Start a new R session and call `library("MASH", lib="/ihme/malaria/[your name]/Rlibs/")`
* The MASH library should load.  You did it!

I know there are other ways to build R packages, but this is the way that I have chosen because it makes the most sense to me.  If, for convenience or preference you want to do something else, here are some other options:

* You can load the MASH_0.1 project within an R studio session (initiated using `rstudio_ide`) and then build the package locally - you may want to do this if you want to install your R libraries in different locations, and have more explicit control over which paths you want to access.
  * Navigate to the "Build" tab on the right
  * Go to the "More" menu
  * Open "Configure Build Tools..."
  * Add `--library=/ihme/code/malaria/dtcitron/Rlibs` to "Build and Reload" additional options
  * Click "Install and Restart" to build the package
* You can also try using the devtools package, which can even install MASH directly from our GitHub repository.  (I haven't yet investigated how to do this.)

<dl><hr></dl>

### MASH Demo - interactive session

After installing the MASH library, try the following to make sure that the library works correctly.  This will demonstrate a workflow for using MASH in an interactive session on the cluster.

1. Start an interactive session using `qlogin`
2. Open Rstudio with `rstudio_ide`
3. Set the working directory:  `setwd("/ihme/malaria/[your name]")`
4. Set your path to include the Rlibs directory where the MASH library is stored:
  `.libPaths(c(.libPaths(), "/ihme/malaria/dtcitron/Rlibs/"))`
5. Open the Macro Emerge test `/MASH-Main/MASH-dev/DavidSmith/MASH_0.1/Test-MACRO-Emerge.R`
6. Step through the first 59 lines, checking that the library loads correctly and the simulation runs and produces plottable output.

<dl><hr></dl>

### MASH Demo - using qsubs

<dl><hr></dl>

### Workflow on the Cluster

We want to always benchmark our work before starting a job on the cluster.

1.  Run a small version of the script locally - make sure the simulation can run without throwing errors.  
2.  Run a small version of the script in an interactive session (qlogin) on cluster-dev.  Make sure that the path dependencies for libraries and everything work ok.
3. Profile the script in preparation for parallelizing using the `qsub` command. (More to follow regarding `qsub` jobs.)
4. Parallelize, using a set of `qsub` commands, or with an array job.  (More to follow regarding parallelizing simulations.)

<dl><hr></dl>

### Some additional shortcuts that you might want
1. Create command line aliases for ssh'ing onto cluster
  * Want to skip having to write out ssh blah blah blah every time you log on to the cluster?
  * On your local machine, add to your `.bash_profile` the following two lines:
    * `alias dev="ssh [login name]@cluster-dev.ihme.washington.edu -XY"`
    * `alias dev="ssh [login name]@cluster-prod.ihme.washington.edu -XY"`
  * Source your `.bash_profile`, and then you can ssh onto either dev or prod very quickly!
  * [Reference](https://stackoverflow.com/questions/15968053/using-alias-in-shell-script)
2. Shortcut password requirements
  * Want to skip having to enter your intricate and ponderous password every time you log in?
  * Shortcut ssh password:
    * Create a public key on your local machine, then put it in `.ssh/authorized_keys` on the cluster login node
    * Use SSH Keygen, following the instructions [here](https://www.tecmint.com/ssh-passwordless-login-using-ssh-keygen-in-5-easy-steps/)
    * Tip: when creating your keys with ssh-keygen (in step 1), enter nothing
  * Shortcut qlogin password entry
    * This procedure is very similar to creating a public key on your local
    machine and saving it on the login node on the remote machine.
    * On the cluster login node (remote), create a pair of keys
    * Move the public key from the remote machine into `.ssh/authorized_keys` on the remote machine!
    * Now, try `qlogin` on dev, and you'll skip past the step where you need to enter a password
