# Usage

- make `.yml` files from the `.example` files in `src/main/resources`
- run:
```sh
sbt run
sbt "run-main trader.TestXchange" | tee -a output.log
sbt "run-main trader.FollowPings" | tee -a output.log
sbt "run-main trader.ScrapeHistory" | tee -a output.log
# check deps
sbt dependency-graph
# make jar
sbt assembly
# run jar
java -jar ./target/scala-2.12/Trader-*.jar | tee -a trader.log
# testing XChange
mvn '-DfailIfNoTests=false' '-DskipIntegrationTests=false' test
mvn '-DfailIfNoTests=false' '-Dtest=org.knowm.xchange.examples.yobit.*' '-DskipIntegrationTests=false' test
```

# TODO

Project scope:
- trading behavior:
	- scrape history, persist, ML
- fix exchanges:
	- Poloniex: gives `ExchangeError`s
	- Yobit: [no AccountService/TradeService](https://github.com/timmolter/XChange/tree/develop/xchange-yobit/src/main/java/org/knowm/xchange/yobit/service), though endpoints [available](https://yobit.net/en/api/)
	- Cryptopia: [not implemented](https://github.com/timmolter/XChange/issues/1596)

Extra:
- wrap Xchange methods in functions returning Try monads to take care of null checks / exception catching
- switch to akka
- don't flip the original pair data, or I'd query for pairs that don't exist
- exchange tests as data, so it knows beforehand which exchanges are missing certain functionality
- make smarter:
	- move across exchanges (when needed/available)
	- cancel orders if they'd no longer expect fill / be profitable
	- move all assets into coin expected to rise most
		- yet spread risk?
- deal with pings without available exchange ticker:
	- use signal as ASK reference to make limit order
	- verify ping by % up
- linalg/ML: tweak params, infer fees/time
