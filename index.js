let csv = require('csv');
let _ = require('lodash');
let gaussian = require('gaussian');
let fs = require('fs');

class WeightManager {
	constructor() {
		this.weights = {};
	}
	
	setWeight(key, weight) {
		this.weights[key] = weight;
	}
	
	getWeight(key) {
		return this.weights[key] || 0;
	}
	
	getWeights() {
		return this.weights;
	}
	
	getKeys() {
		return _.keys(this.weights);
	}
	
	getSimilarity(otherWeightManager) {
		let similarity = 0;
		let keys = _.union(this.getKeys(), otherInterestManager.getKeys());
		let sumOfWeightDifferences = 0;
		keys.forEach((key) => {
			let weightDifference = Math.abs(this.getWeight(key) - otherInterestManager.getWeight(key));
			sumOfWeightDifferences += weightDifference;
		});
		if (keys.length > 0) {
			similarity = sumOfWeightDifferences / keys.length;
		}
		return similarity;
	}
	
	addRandomWeight(key, minWeight, maxWeight, isWeightInteger) {
		let weight = _.random(minWeight, maxWeight, !isWeightInteger);
		this.setWeight(key, weight);
	}
	
	addRandomWeights(keys, keyCount, minWeight, maxWeight, isWeightInteger) {
		let keysToAdd = _.sampleSize(keys, Math.min(keys.length, keyCount));
		keysToAdd.forEach((key) => {
			this.addRandomWeight(key, minWeight, maxWeight, isWeightInteger);
		});
	}
	
}

class RandomDistribution {
	constructor(mean, stddev, isInteger) {
		let variance = stddev * stddev;
		this.distribution = gaussian(mean, variance);
		this.isInteger = isInteger;
	}
	
	getRandomNumber() {
		let randomNumber = this.distribution.ppf(Math.random());
		if (this.isInteger) {
			return Math.round(randomNumber);
		} else {
			return randomNumber;
		}
	}
}

class Person {
	constructor(id) {
		this.id = id;
		this.interestWeightManager = new WeightManager();
		this.opinionWeightManager = new WeightManager(); // opinion of other people
	}
	
	getId() {
		return this.id;
	}
	
	setOpinionWeight(personId, weight) {
		this.opinionWeightManager.setWeight(personId, weight);
	}
	
	getOpinionWeight(personId) {
		return this.opinionWeightManager.getWeight(personId);
	}
	
	setInterestWeight(interestId, weight) {
		this.interestWeightManager.setWeight(interestId, weight);
	}
	
	getInterestWeight(interestId) {
		return this.interestWeightManager.getWeight(interestId);
	}
	
	addRandomInterests(interestIds, count) {
		this.interestWeightManager.addRandomWeights(interestIds, count, -1, 1, true);
	}
	
	addRandomOpinions(personIds, count) {
		this.opinionWeightManager.addRandomWeights(personIds, count, -1, 1, true);
	}
	
	getKnownPersonIds() {
		return this.opinionWeightManager.getKeys();
	}
	
	getOpinions(round) {
		let personIds = this.getKnownPersonIds();
		let opinions = _.map(personIds, (personId) => {
			let emotionalValence = this.getOpinionWeight(personId);
			let opinion = new Opinion(this.getId(), personId, emotionalValence, round);
			return opinion;
		});
		return opinions;
	}
	
	getInterests() {
		return this.interestWeightManager.getKeys();
	}
	
	interact(otherPerson, round) {
		let opinionOfOtherPerson = this.getOpinionWeight(otherPerson.getId());
		let emotionalValence = _.random(-1, 1) + _.random(Math.min(opinionOfOtherPerson, 0), Math.max(opinionOfOtherPerson, 0));
		emotionalValence = Math.min(Math.max(-1, emotionalValence), 1);
		let behavior = new Behavior(this.getId(), otherPerson.getId(), emotionalValence, round);
		this.observeBehavior(behavior);
		otherPerson.observeBehavior(behavior);
		return behavior;
	}
	
	observeBehavior(behavior) {
		if (behavior.fromPersonId === this.id) {
			this.setOpinionWeight(behavior.toPersonId, behavior.emotionalValence);
		} else if (behavior.toPersonId === this.id) {
			this.setOpinionWeight(behavior.fromPersonId, behavior.emotionalValence);
		}
	}
}

class Logger {
	constructor(fileName, columns) {
		this.outputStream = fs.createWriteStream(fileName);
		this.stringifier = csv.stringify({ header: true, columns: columns });
		this.stringifier.pipe(this.outputStream);
	}
	
	log(rowObject) {
		this.stringifier.write(rowObject);
	}
}

class Opinion {
	constructor(fromPersonId, toPersonId, emotionalValence, round) {
		this.fromPersonId = fromPersonId;
		this.toPersonId = toPersonId;
		this.emotionalValence = emotionalValence;
		this.round = round;
	}
}

class OpinionLogger {
	constructor(fileName) {
		let columns = {
			fromPersonId: 'fromPersonId',
			toPersonId: 'toPersonId',
			emotionalValence: 'emotionalValence',
			round: 'round' 
		};
		this.logger = new Logger(fileName, columns);
	}
	
	log(opinion) {
		this.logger.log(opinion);
	}
	
}

class BehaviorLogger {
	constructor(fileName) {
		let columns = {
			fromPersonId: 'fromPersonId',
			toPersonId: 'toPersonId',
			emotionalValence: 'emotionalValence',
			round: 'round' 
		};
		this.logger = new Logger(fileName, columns);
	}
	
	log(behavior) {
		this.logger.log(behavior);
	}

}

class Behavior {
	constructor(fromPersonId, toPersonId, emotionalValence, round) {
		this.fromPersonId = fromPersonId;
		this.toPersonId = toPersonId;
		this.emotionalValence = emotionalValence;
		this.round = round;
	}
}

class Group {
	constructor(personCount, interestCount, behaviorLogger, opinionLogger) {
		this.people = this.generateRandomPeople(personCount, interestCount);
		this.behaviorLogger = behaviorLogger;
		this.opinionLogger = opinionLogger;
	}
	
	generateRandomPeople(personCount, interestCount) {
		let interests = _.range(1, interestCount);
		let personIds = _.map(_.range(1, personCount + 1), (personId) => { return personId + ''; } );
		let relationshipCountDistribution = new RandomDistribution(5, 3, true);
		let people = _.map(personIds, (personId) => {
			let person = new Person(personId);
			let interestsPerPersonCount = 3; //_.random(1, interests.length);
			let relationshipsPerPersonCount = Math.max(0, relationshipCountDistribution.getRandomNumber());
			let otherPersonIds = _.filter(personIds, (pId) => { return pId != personId; });
			person.addRandomOpinions(otherPersonIds, relationshipsPerPersonCount);
			return person;
		});
		return people;
	}
	
	interact(rounds) {
		_.times(rounds, (round) => {
			
			this.logOpinions(round);
			
			// for each person, have them interact with one other random person
			_.forEach(this.people, (person) => {
				
				let otherPerson = _.sample(_.filter(this.people, (p) => { 
					return p.getId() !== person.getId(); 
				}));
					
				let behavior = person.interact(otherPerson, round);		
				this.behaviorLogger.log(behavior);
				
			});
			
			if (round === rounds - 1) {
				this.logOpinions(round + 1);
			}
			
		});
	}
	
	logOpinions(round) {
		_.forEach(this.people, (person) => {
			let opinions = person.getOpinions(round);
			_.forEach(opinions, (opinion) => {
				this.opinionLogger.log(opinion);
			});
		});
	}
}


let personCount = 30;
let interestCount = 3;
let roundCount = 100;

let behaviorLogger = new BehaviorLogger('behaviors.csv');
let opinionLogger = new OpinionLogger('opinions.csv');
let group = new Group(personCount, interestCount, behaviorLogger, opinionLogger);
group.interact(roundCount);
