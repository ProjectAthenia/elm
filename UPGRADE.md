# Athenia App Upgrade Guide

To upgrade from previous version of Athenia please check each version number listed below step by step.

## 0.12.0

Bigger update! This version migrates to webpack version 5, and adds the ability to set default filters in the ModelList.

* config/ - New webpack config created
* .env.example - Added example environment
* package.json - Reworked all dependencies, and scripts
* src/assets/ - New directory
* src/elm/Api.elm - Fixed delete issue
* src/elm/Components/CRUD/ModelForm.elm - Made a bit more general
* src/elm/Components/CRUD/ModelList.elm - Added filters and expanded functionality
* src/elm/Components/CRUD/RootController.elm - Updated for general model form
* src/elm/Page/Admin/Sections/MembershipPlan.elm - Updated for general model form changes
* src/elm/Page/Admin/Sections/User.elm - Updated for general model form changes
* webpack.config.js - Old webpack config removed

## 0.11.1

Bug fix for remaining admin route reference in `src/elm/Components/CRUD/ModelList.elm`.

## 0.11.0

Another minor one. This one adds a new boolean decoder to `src/elm/Utilities/ModelHelpers.elm`

## 0.10.1

Small little wording change, `src/elm/Components/CRUD/ModelForm.elm` update success message changed.

## 0.10.0

Big change! This update completely removes the home page in favor of making the admin index the home page again.

* src/elm/Components/CRUD/ModelForm.elm - URL references updated
* src/elm/Components/CRUD/ModelList.elm - URL references updated
* src/elm/Main.elm - Removed home page references
* src/elm/Page/Admin/Root.elm - URL references updated
* src/elm/Page/Home.elm - Removed
* src/elm/Route.elm - Removed home page references
* src/elm/Utilities/ModelHelpers.elm - Added a new decoder

## 0.9.3

Simple little one! This fixes an issue pertaining to situations were optional config variables are not set. To fix this issue simply copy over `src/index.js`

## 0.9.2

Another little one! This simply fixed a bug in the index searching. To update to this version simply copy over the file `src/elm/Utilities/SearchField.elm`.

## 0.9.1

Simple one! Simply copy over `src/elm/Utilities/ModelHelpers.elm` in order to fix a bug with some model decoders

## 0.9.0

A nice small one! This update adds the ability to save a child model after each child update, adds some sub map functions, and fixes a bug in the model form. To complete this update simply copy over `src/elm/Components/CRUD/ModelForm.elm` and `src/elm/Components/CRUD/RootController.elm`.

## 0.8.0

Another nice sized update! This one adds a new entity payment history and subscription history component. It also adds some reset password functionality for the user CRUD section. It also includes additional improvements to the recent CRUD setup and entity integrations.

* elm.json - Make sure to install the `ryannhg/date-format` elm package
* src/elm/Api.elm - The endpoints related to the user subscription have been updated to use the entity structures
* src/elm/Api/Endpoint.elm - New entity sub endpoints have been added
* src/elm/Components/Entity/PaymentHistory.elm - New Module!
* src/elm/Components/Entity/SubscriptionHistory.elm - New Module!
* src/elm/Components/MembershipPlan/RateHistory.elm - Added proper view for individual rates
* src/elm/Components/User/ResetPasswordButton.elm - New Module!
* src/elm/Models/MembershipPlan/MembershipPlan.elm - Improved decoder
* src/elm/Models/MembershipPlan/MembershipPlanRate.elm - Added the created at timestamp and improved the cost decoder
* src/elm/Models/MembershipPlan/Subscription.elm - Added a number of helper functions, and changed a model field from maybe posix to posix
* src/elm/Models/Payment/LineItem.elm - New Model!
* src/elm/Models/Payment/Payment.elm - New Model!
* src/elm/Models/User/User.elm - Improved subscription functions, and added another encoder.
* src/elm/Page/Admin/Sections/MembershipPlan.elm - Updated initiation of Rate history to pass through the current auth token
* src/elm/Page/Admin/Sections/User.elm - Integrated all three new components
* src/elm/Page/Settings.elm - Added created at field to membership plan rate model creation, and updated API calls for entity changes
* src/elm/Utilities/DateHelpers.elm - Added a format helper function
* src/elm/Utilities/ModelHelpers.elm - Added an improved float decoder
* tests/UnitTests/Models/MembershipPlan/MembershipPlanRateTest.elm - Updated for created_at field being added
* tests/UnitTests/Models/MembershipPlan/SubscriptionTest.elm - Updated for model changes
* tests/UnitTests/Models/Payment/LineItemTest.elm - New test!
* tests/UnitTests/Models/Payment/PaymentTest.elm - New test!
* tests/UnitTests/Models/User/UserTest.elm - Updated for model changes

## 0.7.0

Quality of life update! This is an accumulative update with a number of improvements added.

* src/elm/Api.elm - Improved some error messages
* src/elm/Api/Endpoint.elm - Added a `membershipPlanRates` endpoint
* src/elm/Components/CRUD/ModelForm.elm - Passed in the api url to the child init function, fixed some error text, and started displaying toasts.
* src/elm/Components/CRUD/ModelForm/SelectField.elm - Added a default option
* src/elm/Components/CRUD/RootController.elm - Removed toasts, and fixed a bug with the model list updating.
* src/elm/Components/CRUD/SharedConfiguration.elm - Added apiUrl to shared config
* src/elm/Components/MembershipPlan/RateHistory.elm - Completed the module
* src/elm/Models/Error.elm - Fixed some error messages
* src/elm/Models/MembershipPlan/MembershipPlan.elm - Improved current cost decoder
* src/elm/Models/MembershipPlan/MembershipPlanRate.elm - Added page decoder
* src/elm/Page/Admin/Sections/MembershipPlan.elm - Integrated the rate history throughout
* src/elm/Page/Admin/Sections/User.elm - Updated init function for new parameter
* src/elm/Page/Login.elm - Fixed redirect bug

## 0.6.0

Huge update! The main crux of this update is a massive improvement fo the current CRUD setup. Most of the create and update logic that previously had to be reimplemented in every admin section is now taken care of by the core. This will cause a near rewrite of any existing sections. 

This also updates the installation to be compatible with the version 0.40 of the API, and that update will be required before this can be implemented. There are also a lot of little updates that have been made.

* .env.example - A new storage key variable has been added
* .gitignore - .DS_Store has been added
* src/elm/Api/Endpoint.elm - A roles endpoint has been added, url is now exposed, and the API version number has been removed from the url building.
* src/elm/Api/Group.elm - New module
* src/elm/Components/CRUD/ModelForm.elm - Almost completely redone
* src/elm/Components/CRUD/ModelForm/Input.elm - initialState renamed to configure
* src/elm/Components/CRUD/ModelForm/NumberField.elm - Class name added to div
* src/elm/Components/CRUD/ModelForm/SelectField.elm - New module
* src/elm/Components/CRUD/ModelForm/TextAreaField.elm - Class name added to div
* src/elm/Components/CRUD/ModelForm/TextField.elm - View function cleaned up quite a bit
* src/elm/Components/CRUD/ModelForm/ToggleField.elm - View function cleaned up quite a bit
* src/elm/Components/CRUD/ModelList.elm - Almost completely redone
* src/elm/Components/CRUD/RootController.elm - Almost completely redone
* src/elm/Components/CRUD/SharedConfiguration.elm - New module
* src/elm/Components/MembershipPlan/RateHistory.elm - New module
* src/elm/Main.elm - Integrated admin, probably best to copy over and run a compare
* src/elm/Modals/ArticleHistoryBrowser.elm - Updated for user name field change
* src/elm/Models/Error.elm - Added some helper functions
* src/elm/Models/MembershipPlan/MembershipPlan.elm - Almost rewrote, best to do a compare
* src/elm/Models/MembershipPlan/MembershipPlanRate.elm - Made membership_plan optional
* src/elm/Models/User/User.elm - Almost rewrote, best to do a compare
* src/elm/Models/Wiki/Article.elm - Updated decoder for potential option user id
* src/elm/Page.elm - Added admin
* src/elm/Page/Admin/ - New group
* src/elm/Page/Article/Viewer.elm - Updated for potential optional user id
* src/elm/Page/Home.elm - Fixed potential login loop bug
* src/elm/Page/Login.elm - Took user to admin home, and updated user model for structure changes
* src/elm/Page/Profile.elm - Updated user name reference
* src/elm/Page/Settings.elm - Updated for user model changes
* src/elm/Route.elm - Added admin routes
* src/elm/Utilities/ModelHelpers.elm - Add additional helpers for generic models
* src/elm/Utilities/SearchField.elm - Added option for no search
* src/elm/Viewer.elm - Minor cleanup to imports
* src/ports/athenia.js - Fixed potential bug cause ports to stop execution
* src/scss/colors.scss - More colors!
* src/scss/components/crud/ - New section
* src/scss/main.scss - Imported new sections
* src/scss/pages/admin/ - New section
* tests/UnitTests/MainTest.elm - Updated for admin 
* tests/UnitTests/Models/MembershipPlan/MembershipPlanRateTest.elm - Updated for membership plan being optional
* tests/UnitTests/Models/MembershipPlan/MembershipPlanTest.elm - Updated for id being optional
* tests/UnitTests/Models/MembershipPlan/SubscriptionTest.elm - Updated for fields being optional
* tests/UnitTests/Models/PageTest.elm - Updated for user name field changes
* tests/UnitTests/Models/User/UserTest.elm - Updated for user field changes
* tests/UnitTests/Models/Wiki/ArticleTest.elm - Updated for user field changes
* tests/UnitTests/Models/Wiki/IterationTest.elm - Updated for user field changes

## 0.5.0

This is a minor little update that simply adds some more helper functions that you can use within your apps. To complete this update update the following files.

* src/elm/Api.elm - New Helper function that makes it easier to display errors from an error model.
* src/elm/Models/Role.elm - Added a new Page type
* src/elm/Models/User/User.elm - Added a new page type, a login model, a function to help determine if a user is an administrator, and encoded roles if they are set.
* src/elm/Utilities/ModelHelpers.elm - Updated the encode ids function to return a completely encoded value.
* tests/UnitTests/Models/User/UserTest.elm - Added a new test to make sure that roles were encoded properly.
* webpack.config.js - Updated build location settings

## 0.4.0

CRUD! This adds a huge new feature that will allow you to quickly add CRUD modules that will allow someone the ability to manage data in any Athenia based API. Most of this is brand new functionality, but there are a few updates made to existing files that will need to be made as well. This update also officially upgrades the core from elm 0.19.0 to 0.19.1, so make sure to update both the elm.json and the package.json.

### New Files

* src/elm/Components/AudioPlayer.elm
* src/elm/Components/CRUD/
* src/elm/Components/Toast.elm
* src/elm/Modals/Confirmation.elm
* src/elm/Models/Asset.elm
* src/elm/Models/FileUpload.elm
* src/elm/Models/Status.elm
* src/elm/Ports/FileReader.elm
* src/elm/Utilities/Expands.elm
* src/elm/Utilities/Order.elm
* src/elm/Utilities/SearchField.elm
* tests/UnitTests/Models/AssetTest.elm

### Existing Files

* src/elm/Api.elm 
* src/ports/athenia.js

## 0.3.1

This fixes a potential bug with the article editor displaying the incorrect iteration source. To finish this update simply copy over the following files.

* src/elm/Models/Wiki/Article.elm
* src/elm/Page/Article/Editor.elm
* tests/UnitTests/Models/Wiki/ArticleTest.elm

## 0.3.0

This update is a pretty big update that adds a ton of utility functions. To start, make sure that list extra `elm-community/list-extra` is installed. Then update the following files.

* src/elm/Models/Role.elm
* src/elm/Models/User/User.elm
* src/elm/Utilities/ModelHelpers.elm
* src/elm/Utilities/StringHelper.elm
* src/index.js
* webpack.config.js
* tests/UnitTests/Models/User/UserTest.elm
* tests/UnitTests/Utilities/StringHelperTest.elm

Then copy over the `src/ports/` directory, and `tests/UnitTests/Utilities/ModelHelpersTest.elm` test to finish the update.

## 0.2.1

This is a simple security update. Start off by adding `"uglifyjs-webpack-plugin": "^1.3.0",` to the dev dependencies, and then run update. The update `webpack.config.js` to the version found in this project.
