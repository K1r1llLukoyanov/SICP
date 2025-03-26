class Person{
	constuctor(name){
		this.name = name;
	}
	get_name(){
		return this.name;
	}
}


const square = (value) => value*value;


function plus(plus_value){
	return (value) => value + plus_value;
}


(function main(){
	let person = new Person("Kirill");
	console.log(person.get_name());
})();


