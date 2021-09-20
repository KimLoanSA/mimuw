package validation;

import petrinet.PetriNet;
import petrinet.Transition;

import java.util.*;


class ReachTest
{
    private PetriNet<String> petrinet;

    private volatile boolean running;
    private class Writer implements Runnable
    {
        private Collection<Transition<String>> transitions;


        Writer(Collection<Transition<String>> trans)
        {
            transitions = trans;
        }

        private void writeSth()
        {
            System.out.print(Thread.currentThread().getName());
            System.out.print('.');
        }

        @Override
        public void run()
        {
            int counter = 0;
            running = true;
            while(running)
            {
                try
                {
                    petrinet.fire(transitions);
                    counter++;
                }
                catch (InterruptedException e){
                    //thread interrupted due to end of work
                }
            }

            System.out.println(Thread.currentThread().getName() + " " + counter + " obrotow.");
        }
    }


    private class Reader implements Runnable
    {
        private Collection<Transition<String>> transitions;
        int counter = 0;
        Reader(Collection<Transition<String>> trans)
        {
            transitions = trans;
        }

        @Override
        public void run()
        {
            running = true;
            while(running)
            {
                petrinet.reachable(transitions);
                counter++;
            }

            System.out.println(Thread.currentThread().getName() + " " + counter + " obrotow.");
        }
    }

    private void generateTransitions(String me, String y, String z, String central,
                                     Collection<Transition<String>> beg,
                                     Collection<Transition<String>> end)
    {
        {
            Collection<String> inhibitors = new ArrayList<>();
            Collection<String> resets = new ArrayList<>();
            Map<String, Integer> in = new HashMap<>();
            Map<String, Integer> out = new HashMap<>();

            inhibitors.add(central);
            inhibitors.add(z);

            out.put(central, 1);
            out.put(z, 1);

            in.put(me, 1);

            beg.add(new Transition<>(in, resets, inhibitors, out));
        }

        {
            Collection<String> inhibitors = new ArrayList<>();
            Collection<String> resets = new ArrayList<>();
            Map<String, Integer> in = new HashMap<>();
            Map<String, Integer> out = new HashMap<>();

            inhibitors.add(central);
            inhibitors.add(y);

            out.put(central, 1);
            out.put(y, 1);

            in.put(me, 1);

            beg.add(new Transition<>(in, resets, inhibitors, out));
        }
        //end
        {
            Collection<String> inhibitors = new ArrayList<>();
            Collection<String> resets = new ArrayList<>();
            Map<String, Integer> in = new HashMap<>();
            Map<String, Integer> out = new HashMap<>();

            resets.add(central);

            end.add(new Transition<>(in, resets, inhibitors, out));
        }
    }

    ReachTest()
    {
        final String placeA = "wefwf";
        final String placeB = "wgtfergerg";
        final String placeC = "423t3tg3g3g";
        final String placeCentral = "werfgewrgegeg";

        Map<String, Integer> initial = new HashMap<>();
        initial.put(placeA,1);
        initial.put(placeB,1);
        initial.put(placeC,0);
        initial.put(placeCentral,0);

        petrinet = new PetriNet<>(initial, true);
        Collection<Transition<String>> begin_a = new ArrayList<>();
        Collection<Transition<String>> begin_b = new ArrayList<>();
        Collection<Transition<String>> begin_c = new ArrayList<>();
        Collection<Transition<String>> end_a = new ArrayList<>();
        Collection<Transition<String>> end_b = new ArrayList<>();
        Collection<Transition<String>> end_c = new ArrayList<>();

        generateTransitions(placeA, placeB, placeC, placeCentral, begin_a, end_a);
        generateTransitions(placeB, placeA, placeC, placeCentral, begin_b, end_b);
        generateTransitions(placeC, placeB, placeA, placeCentral, begin_c, end_c);


        Collection<Transition<String>> allTransitions = new ArrayList<>();allTransitions.addAll(begin_a);allTransitions.addAll(begin_b);allTransitions.addAll(begin_c);allTransitions.addAll(end_a);allTransitions.addAll(end_b);allTransitions.addAll(end_c);


        Thread t1 = new Thread(new Writer(allTransitions), "W1");
        Thread t2 = new Thread(new Writer(allTransitions), "W2");
        Thread t3 = new Thread(new Writer(allTransitions), "W3");

        Thread t4 = new Thread(new Writer(allTransitions), "R1");
        Thread t5 = new Thread(new Writer(allTransitions), "R2");
        Thread t6 = new Thread(new Writer(allTransitions), "R3");

        t1.start();
        t2.start();
        t3.start();

        t4.start();
        t5.start();
        t6.start();

        try
        {
            Thread.sleep(10000);
        }catch(InterruptedException ex)
        {
            System.out.println("Interrupted");
        }
        running = false;
        System.out.println("Finished");
    }

    public static void main(String[] args) {
        new ReachTest();
    }
}
