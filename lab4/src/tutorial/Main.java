package tutorial;

import java.util.Arrays;

public class Main {

    public static void main(String[] args) {
        Integer[] a = new Integer[5];
        for (int i = 0; i <a.length; i++) {
            a[i] = i;
        }
        printArray(a);
        Integer[] b = Arrays.stream(a).map(i -> i + 1).toArray(Integer[]::new);
        printArray(b);

        Integer[] c = Arrays.stream(a).filter(i -> i % 2 == 0).toArray(Integer[]::new);
        printArray(c);

        Integer s = Arrays.stream(a).reduce(0, (m, n) -> m + n);
        System.out.println(s);

        Integer[] d = Arrays.stream(a).filter(i -> i % 2 == 0).map(i -> i * 2).toArray(Integer[]::new);
        printArray(d);

        Double[] randoms = new Double[10000000];
        for (int i = 0; i < randoms.length; i++) {
            randoms[i] = Math.random();
        }
        System.out.println();
        long startTime = System.currentTimeMillis();
        Double[] par = Arrays.stream(randoms).parallel().map(i -> i*Math.sin(i)/i).toArray(Double[]::new);
        long parallel = System.currentTimeMillis() - startTime;
        for (int i = 0; i < randoms.length; i++) {
            randoms[i] = Math.random();
        }
        startTime = System.currentTimeMillis();
        Double[] seq = Arrays.stream(randoms).map(i -> i*Math.sin(i)/i).toArray(Double[]::new);
        long sequential = System.currentTimeMillis() - startTime;
        System.out.println("Sequential " + sequential + "ms");
        System.out.println("Parallel " + parallel + "ms");
        System.out.println(seq[0] + " " + par[0]);
    }


    private static void printArray(Number[] arr) {
        for (int i = 0; i < arr.length; i++) {
            System.out.print(arr[i] + " ");
        }
        System.out.println("");
    }
}
