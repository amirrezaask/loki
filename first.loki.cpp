#include "cstdio"
#include "string"

struct Human {
	int age;
std::string name;
};
union Result {
	void* Ok;
void* Ko;
int Unknown;
};
int main() {
	const bool b1 = true;
	const bool b2 = false;
	const std::string s = "Amirreza";
	const Human amirreza = {
.age=24,
.name="amirreza"};
	const Human parsa = {
.age=17,
.name="parsa"};
	printf("amirreza age is %d\n",amirreza.age);
	printf("parsa age is %d\n",parsa.age);
	if (b1) {
	printf("b1 is true\n");
	};
	if (b2) {
	printf("b2 is true\n");
	} else {
	printf("b2 is false\n");
};
	return 0;
}