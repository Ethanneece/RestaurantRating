build:
	scalac -cp .:ComputeAll.jar Recommender.scala
run:
	./run.sh
clean:
	@rm -f *.class
