# rodash ~ rOpenSci Dashboard

This app is built on [Dashing](http://shopify.github.com/dashing), from [Shopify.com](Shopify.com). In their words, "Dashing is a Sinatra based framework that lets you build beautiful dashboards".

The app is hosted on Heroku.

See the dashboard here [http://rodash.herokuapp.com/sample](http://rodash.herokuapp.com/sample).  You can move the tiles (or in their words, widgets) around - put them in new places, etc. Here's a screenshot:

![](https://raw.github.com/ropensci/rodash/master/assets/images/app_sshot.png)

## Instructions for modifying, pushing to Github/Heroku

### Modify

See [the Dashing Github page](http://shopify.github.com/dashing) in general for ideas. They have some demo pages to get ideas [here](http://dashingdemo.herokuapp.com/sample) and [here](http://dashingdemo.herokuapp.com/sampletv).

You can modify the default 'apis' app, or create a new app inside the /dashboards/ folder. 

##### Tiles
Each tile in the app is a 'widget'. Go to /jobs/filename.rb to edit a particular tile, or within that folder to add a new tile, etc. 

##### Styling
Files of interest:

+ /assets/javascripts/application.coffee
+ /assets/stylesheets/application.scss

##### Other files of interest:

+ /config.ru (setting configuration settings)
+ Dashboards are in /dashboards/ (the default one is called apis.erb)

### Push to GitHub or to Heroku

#### Github

As you normally would commit and push.

#### Heroku

If you just pushed to Github, files are already committed for Heroku, so we just need to bundle up the app, and push to Heroku.  

```
bundle install
git push heroku master
```  

### Look at new site at [http://rodash.herokuapp.com/apis](http://rodash.herokuapp.com/apis).