# Twitter User Recommender
Twitter user recommendation system written in Prolog. Provide a query based on content and type of tweet and we'll recommend you some popular users whose tweets match your query!

## To Run
- `swipl`
- `[userRecommenderSystem].`
- set Bearer Token for Twitter API: `setenv("TWITTER_BEARER_TOKEN", <bearer_token>).`
- to see the full list of user accounts: `set_prolog_flag(answer_write_options,[max_depth(0)]).`
- run the program: `q(Ln, RecommendedUsers, NumResults).`
- Input a query to get a recommended list of user accounts
_The query must be structured according to the following grammar: (\w [or|and]? \w)* (filter [[media|links|images|native_video|retweets|verified|quote] [and|or]?]*) ?_

## Sample Queries
- grumpy cat or cat filter media or verified
- nasa and space filter media
- meme and funny
- politics
- xyz filter verified
- filter media and retweets

### Invalid Queries
- grumpy or media
- grumpy or filter
- cat filter something
- filter
- filter media and
- filter media retweets
- ; nasa ;
- or nasa filter media
- grumpy cat filter
- grumpy and and cat