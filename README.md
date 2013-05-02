<<<<<<< HEAD
neotoma
========

This package is a programmatic R interface to the [Neotoma Paleoecological Database](http://www.neotomadb.org/). 

#### Neotoma API docs: [http://api.neotomadb.org/doc/](http://api.neotomadb.org/doc/)

#### Authors: [Simon Goring](simon.j.goring@gmail.com)

#### Quick start

```r

```
=======
# rodash ~ rOpenSci Dashboard

This app is built on [Dashing](http://shopify.github.com/dashing), from [Shopify.com](Shopify.com). In their words, "Dashing is a Sinatra based framework that lets you build beautiful dashboards".

The app is hosted on Heroku.

See the dashboard here [http://rodash.herokuapp.com/roapi](http://rodash.herokuapp.com/roapi).  You can move the tiles (or in their words, widgets) around - put them in new places, etc. Here's a screenshot:

![](https://raw.github.com/ropensci/rodash/master/assets/images/app_sshot.png)

## Instructions for modifying, pushing to Github/Heroku

### Modify

See [the Dashing Github page](http://shopify.github.com/dashing) in general for ideas. They have some demo pages to get ideas [here](http://dashingdemo.herokuapp.com/sample) and [here](http://dashingdemo.herokuapp.com/sampletv).

You can modify the default 'apis' app, or create a new app inside the /dashboards/ folder. 

##### Tiles
Each tile in the app is a 'widget'. Go to /jobs/filename.rb to edit a particular tile, or within that folder to add a new tile, etc. 

If you add a new tile you need to add a `<li>` element within a dashboard file (e.g., /dashboards/roapi.erb) for it to show up, like this:

```html
<li data-row="1" data-col="1" data-sizex="1" data-sizey="1">
      <div data-id="nameofwidget" data-view="datatype" data-title="whatevername"></div>
</li>
```

##### Styling
Files of interest:

+ /assets/javascripts/application.coffee
+ /assets/stylesheets/application.scss

##### Other files of interest:

+ /config.ru (setting configuration settings)
+ Dashboards are in /dashboards/ (the default one is called apis.erb)

##### Looking at changes locally

Go to the repo folder in terminal, and run `dashing start`, then point browser to [http://localhost:3030/](http://localhost:3030/)

### Push to GitHub/Heroku

#### Github

As you normally would commit and push.

#### Heroku

If you just pushed to Github, files are already committed for Heroku, so we just need to bundle up the app, and push to Heroku.  If you didn't push to github, remember to add and commit files first, then do the below.

```
bundle install
git push heroku master
```  

### Look at new site at [http://rodash.herokuapp.com/apis](http://rodash.herokuapp.com/apis).
>>>>>>> 5f7b5290ff4113e9e8fb1b8dcb14afe196ea2064
