public class J1_1_AmbiguousName_AccessResultFromMethod{

    public int i;

    public J1_1_AmbiguousName_AccessResultFromMethod(int j){
	i = j;
    }

    public J1_1_AmbiguousName_AccessResultFromMethod inc(){
	return new J1_1_AmbiguousName_AccessResultFromMethod(i+1);
    }

    public static int test(){
	return new J1_1_AmbiguousName_AccessResultFromMethod(120).inc().inc().inc().i;
    }

}
