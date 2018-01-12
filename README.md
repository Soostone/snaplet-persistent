[![Build Status](https://travis-ci.org/Soostone/snaplet-persistent.svg?branch=master)](https://travis-ci.org/Soostone/snaplet-persistent)

This package makes it easier to integrate your snap applications with databases
using persistent. It also includes a backend for the auth snaplet defining how
users should be stored in your database.

Adding the snaplet to your application is simply a matter of adding the snaplet
to your application’s state data type and initializing it appropriately in your
application initializer. After you do this, when you run your application for
the first time, the snaplet will automatically install a default configuration
file that looks like this.

postgre-con-str = "host='localhost' dbname='snap-test' user='guest' password='password1'"
postgre-pool-size = 3

Then you can edit this file as you see fit. The auth backend works similarly
with its own set of config options.
