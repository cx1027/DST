package nxcs.testbed;


import nxcs.*;
import nxcs.common.MazeBase;
import nxcs.common.MazeParameters;
import nxcs.utils.HyperVolumn;
import nxcs.utils.ParetoCalculatorSkew;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;

import java.awt.*;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;

public class MazeRunner {

    public static void main(String[] args) throws IOException {

        MazeParameters mp = new MazeParameters();
        NXCSParameters np = new NXCSParameters();

        mp.totalTrailCount = 10;
        mp.finalStateUpperBound =3200;
        mp.resultInterval = 50;
        mp.logLowerFinalState = true;
        mp.logFolder = "log/maze1/csv/";
        //0:pointMatch, 1:stateMatch, 2:bothMatch, 3:oneMatch
        mp.method = 0;


        np.N = 150000;
        np.stateLength = 24;
        np.numActions = 4;
        np.rho0 = 1000;
        np.pHash = 0.01;
        np.gamma = 0.85;
        np.crossoverRate = 0.8;
        np.mutationRate = 0.03;
        np.thetaMNA = 4;
        np.thetaGA = 800;
        // np.thetaGA = 0;
        // np.e0 = 0.05;
        np.e0 = 0.05;
        np.thetaDel = 200;
        np.doActionSetSubsumption = false;
        np.doGASubsumption = false;
        np.initialPrediction = 10;

        //initialize weights
        np.weights = new ArrayList<Point>();
//            np.weights.add(new Point(0, 10));
//            np.weights.add(new Point(1, 9));
//            np.weights.add(new Point(2, 8));
//            np.weights.add(new Point(3, 7));
//            np.weights.add(new Point(4, 6));
        np.weights.add(new Point(5, 5));
//            np.weights.add(new Point(6, 4));
//            np.weights.add(new Point(7, 3));
//            np.weights.add(new Point(8, 2));
//            np.weights.add(new Point(9, 1));
//            np.weights.add(new Point(10, 0));

        //initialize reward
//            np.obj1 = new int[]{10, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000};
        np.obj1 = new int[]{100};


        MazeBase maze = null;

        try {
            //initialize and run
            mp.mazeFile = "data/DSTorigin.txt";
            mp.rewardFile = "rewards/DSTfullUpdate.json";
            maze = new dst_weighted_sum(mp.mazeFile);
            maze.initialize(mp, np, parseRewardFile(mp.rewardFile), new HyperVolumn(), new ParetoCalculatorSkew()).run();
        } catch (Exception e) {
            System.out.println(e.toString());
        }


    }

    private static ArrayList<Hashtable<Point, ActionPareto>> parseRewardFile(String rewardFile) throws Exception {
        ArrayList<Hashtable<Point, ActionPareto>> ret = new ArrayList<>();
        JSONParser parser = new JSONParser();
        ClassLoader classLoader = new MazeRunner().getClass().getClassLoader();
        File file = new File(classLoader.getResource(rewardFile).getFile());

        Object obj = parser.parse(new FileReader(file));

        JSONArray rewardArray = (JSONArray) obj;
        System.out.println(rewardArray);
        //System.out.println(jsonObject.get("name"));
        // loop array
        Iterator<JSONObject> iterator = null;
        Iterator<JSONObject> riterator = rewardArray.iterator();
        while (riterator.hasNext()) {
            JSONObject t = riterator.next();
            JSONArray msg = (JSONArray) t.get("rewards");
            iterator = msg.iterator();
            ret.add(parseReward(iterator));
        }

        return ret;
    }


    private static Hashtable<Point, ActionPareto> parseReward(Iterator<JSONObject> rewards) {
        Hashtable<Point, ActionPareto> ret = new Hashtable<Point, ActionPareto>();
        while (rewards.hasNext()) {
            JSONObject t = rewards.next();

            String[] loc = ((String) t.get("location")).split("\\|");
            String[] reward = ((String) t.get("reward")).split("\\|");

            Point p = new Point(Integer.parseInt(loc[0]), Integer.parseInt(loc[1]));
            ActionPareto qreward = new ActionPareto(new Qvector(Double.parseDouble(reward[0]), Double.parseDouble(reward[1])), 0);
            ret.put(p, qreward);
        }

        return ret;
    }
}