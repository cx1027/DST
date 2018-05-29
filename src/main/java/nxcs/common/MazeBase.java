package nxcs.common;

import com.google.gson.Gson;
import nxcs.*;
import nxcs.moead.MOEAD;
import nxcs.stats.StepSnapshot;
import nxcs.stats.StepStatsLogger;
import nxcs.utils.HyperVolumn;
import org.apache.log4j.Logger;

import java.awt.*;
import java.io.*;
import java.util.*;
import java.util.List;
import java.util.stream.Collectors;


/**
 * Represents a maze problem which is loaded in from a file. Such a file should
 * contain a rectangular array of 'O', 'T' and 'F' characters. A sample is given
 * in Mazes/Woods1.txt
 */
public abstract class MazeBase implements Environment, ITrace {

    protected MazeParameters mp;
    protected NXCSParameters np;

    public File mazeFile;
    protected int finalStateCount;

    public static List<Integer> act = new ArrayList<Integer>();
    /**
     * The current position of the agent in the maze
     */
    public int x, y;
    /**
     * The raw characters in the maze
     */
    private char[][] mazeTiles;
    /**
     * The map from characters to their binary encodings used in states and
     * conditions
     */
    private Map<Character, String> encodingTable;
    /**
     * A list of points representing locations we can safely move the agent to
     */
    public ArrayList<Point> openLocations;
    /**
     * A list of points representing the final states in the environment
     */
    public List<Point> finalStates;
    private ArrayList<Reward> rewardGrid;
    /**
     * A list which maps the indices to (delta x, delta y) pairs for moving the
     * agent around the environment
     */
    protected List<Point> actions;
    /**
     * The number of timesteps since the agent last discovered a final state
     */
    protected int stepCount;

    public List<Hashtable<Point, ActionPareto>> positionRewards;
    protected Hashtable<Point, ActionPareto> currentPositionReward;

    protected final static Logger logger = Logger.getLogger(MazeBase.class);
    protected final static Logger statLogger = Logger.getLogger("statlogger");

    private Gson gson = new Gson();
    private HyperVolumn hyperVolumnCalculator;
    private IParetoCalculator paretoCalculator;

    /**
     * Loads a maze from the given maze file
     *
     * @param mazeFile The filename of the maze to load
     * @throws IOException On standard IO problems
     */
    public MazeBase(String mazeFile) throws IOException {
        this(new File(mazeFile));
    }

    /**
     * Loads a maze from the given maze file
     *
     * @param f The file of the maze to load
     * @throws IOException On standard IO problems
     */
    public MazeBase(File f) throws IOException {
        this.mazeFile = f;
    }

    private double[] getRewardWeight() {
        double[] retRewardWeight = new double[this.currentPositionReward.size()];
        int idx = 0;
        for (Point p : this.currentPositionReward.keySet()) {
            retRewardWeight[idx] = this.currentPositionReward.get(p).getPareto().get(1);
            idx++;
        }
        return retRewardWeight;
    }

    public void run() throws Exception {
        List<String> choicestateList = new ArrayList<>();
        String choice = "#1#";
        for (int i = 0; i < choice.length(); i++) {
            char tchar = choice.charAt(i);
            if (tchar == '#') {
                char[] myNameChars = choice.toCharArray();
                myNameChars[i] = '0';
                choicestateList.add(String.valueOf(myNameChars));
                myNameChars[i] = '1';
                choicestateList.add(String.valueOf(myNameChars));
            }
        }
        try {

            boolean logged = false;

            // picture: finalStateUpperBound / 20) / 10 * 10 should be 20
            int chartXInterval = ((this.mp.finalStateUpperBound / this.mp.numOfChartBars) > 10)
                    ? (this.mp.finalStateUpperBound / this.mp.numOfChartBars) / 10 * 10 : 10;

            StepStatsLogger stepStatsLogger = new StepStatsLogger(chartXInterval, 0);
            //Loop weights
            for (Point pweight : this.np.weights) {

                //Loop:diff final reward for obj1
                for (Hashtable<Point, ActionPareto> reward : this.positionRewards) {

                    //set reward for each round
                    this.currentPositionReward = reward;
                    double[] targetWeight = this.getRewardWeight();

                    //how many times a same setting run, then to avg for the result
                    //totalCalcTimes:how many runs want to avg, here set 1 to ignor this loop
                    for (int trailIndex = 0; trailIndex < this.mp.totalTrailCount; trailIndex++) {
                        NXCS nxcs = new NXCS(this, this.np, this.paretoCalculator, mp);

                        //initialize MOEAD
                        MOEAD moeadObj = new MOEAD(this);
                        moeadObj.popsize = 25;
                        moeadObj.neighboursize = 3;
                        moeadObj.TotalItrNum = 250;
                        moeadObj.initialize(this.openLocations, this.np, nxcs);
                        nxcs.setMoead(moeadObj);

//                        initialize the classifiers with moead weights
//                        nxcs.generateCoveringClassifierbyWeight(this.openLocations, moeadObj.weights, this.np);

                        this.resetPosition();

                        this.finalStateCount = 1;


                        logger.info(String.format("######### begin to run of: Weight:%s - first reward:%s - Trail#: %s ",
                                targetWeight, this.np.obj1[0], trailIndex));


                        while (this.finalStateCount < this.mp.finalStateUpperBound) {
                            Point from = this.getCurrentLocation();
                            //run each step
                            nxcs.runIteration(finalStateCount, this.getState(), this.getCurrentLocation(), targetWeight, this.np.obj1[0], moeadObj.getWeights(), mp.method);
                            logger.debug(String.format("Trail:%d, finalStateCount:%d, [%s]=>[%s]", trailIndex, finalStateCount, from, this.getCurrentLocation()));

//                            if (finalStateCount > 2497) {
//                                //logger.info("print classifiers at finalstatecount: " + finalStateCount);
//                                this.printOpenLocationClassifiers(finalStateCount, nxcs, weight, this.np.obj1[obj_num]);
//                            }

                            //if reach final state
                            if (this.isEndOfProblem(this.getState())) {
                                statLogger.fatal(String.format("%d,%d,%d,%d,%d,%d,%d,%d", this.finalStateCount, nxcs.s1, nxcs.s2, nxcs.s3, nxcs.s4, nxcs.s5, nxcs.s6, nxcs.population.size()));
                                this.resetPosition();
                                finalStateCount++;

                                logged = false;
                            }

                            //test algorithm if meet the test condition: 1000 arrived final states
                            if (this.isTraceConditionMeet() && !logged) {
                                // test algorithm
                                logger.info("testing process: Trained on " + finalStateCount + " final states");


                                this.trace(moeadObj, nxcs, stepStatsLogger, trailIndex, targetWeight, this.np.obj1[0], finalStateCount, mp.method);
                                this.resetPosition();

                                logged = true;
                                stepStatsLogger.writeLogAndCSVFiles_TESTING(
                                        String.format("log/%s - %s - Trial %d - TRIAL_NUM - %d - TEST.csv",
                                                this.getClass().getName(), this.mp.fileTimestampFormat, 0, this.np.N),
                                        String.format("log/datadump/%s - %s - Trail %d-<TIMESTEP_NUM> - %d.log", "MOXCS",
                                                this.np.obj1[0], 0, this.np.N));
                            }//endof log
                        } // endof z loop
                        this.printOpenLocationClassifiers(this.finalStateCount, nxcs, moeadObj.weights, targetWeight[0], mp.method);

                        //write result to csv
//                        stepLogger.writeLogAndCSVFiles(
////                                    String.format("log/%s/%s/%s - %s - Trial %d - <TRIAL_NUM> - %d.csv", "MOXCS",
////                                    "MAZE4", weight, this.np.obj1[obj_num], trailIndex, this.np.N),
//                                String.format("log/%s/%s - %s - Trial %d - TRIAL_NUM - %d.csv", "MOXCS",
//                                        "Train", this.mp.fileTimestampFormat, trailIndex, this.np.N),
//                                String.format("log/datadump/%s - %s - Trail %d-<TIMESTEP_NUM> - %d.log", "MOXCS",
//                                        this.np.obj1[obj_num], trailIndex, this.np.N));

//                        stepStatsLogger.writeLogAndCSVFiles_TESTING(
//                                String.format("log/%s/%s - %s - Trial %d - TRIAL_NUM - %d - TEST.csv", "MOXCS",
//                                        "Train", this.mp.fileTimestampFormat, trailIndex, this.np.N),
//                                String.format("log/datadump/%s - %s - Trail %d-<TIMESTEP_NUM> - %d.log", "MOXCS",
//                                        this.np.obj1[obj_num], trailIndex, this.np.N));
                        logger.info("End of trail:" + trailIndex);
                        logger.info(String.format("NXCS:s1=%d, s2=%d, s3=%d, s4=%d, s5=%d", nxcs.s1, nxcs.s2, nxcs.s3, nxcs.s4, nxcs.s5));
                    } // totalTrailCount loop

                    logger.info(String.format("End of %d/%d, objective: objective[%d]=%d", finalStateCount, this.mp.finalStateUpperBound, 0, this.np.obj1[0]));
                } // action selection loop

            }
        } catch (Exception e) {
            e.printStackTrace();
            throw e;
        } finally {
        } // endof try
    }

    public List<Reward> getRewardGrid() {
        return rewardGrid;
    }

    /**
     * Resets the agent to a random open position in the environment
     */
    public void resetPosition() {
        Point randomOpenPoint = XienceMath.choice(openLocations);
        x = randomOpenPoint.x;
        y = randomOpenPoint.y;
        stepCount = 0;
    }

    public void resetToSamePosition(Point xy) {
        x = xy.x;
        y = xy.y;
        stepCount = 0;
    }


    /**
     * Returns the two-bit encoding for the given position in the maze
     *
     * @param x The x position in the maze to get
     * @param y The y position in the maze to get
     * @return The two-bit encoding of the given position
     */
    public String getEncoding(int x, int y) {
        if (x < 0 || y < 0 || y >= mazeTiles.length || x >= mazeTiles[0].length) {
            return encodingTable.get(null);
        } else {
            return encodingTable.get(mazeTiles[y][x]);

        }
    }

    /**
     * Calculates the 16-bit state for the given position, from the 8 positions
     * around it
     *
     * @param x The x position of the state to get the encoding for
     * @param y The y position of the state to get the encoding for
     * @return The binary representation of the given state
     */
    public String getStringForState(int x, int y) {
        StringBuilder build = new StringBuilder();
        for (int dy = -1; dy <= 1; dy++) {
            for (int dx = -1; dx <= 1; dx++) {
                if (dx == 0 && dy == 0)
                    continue;
                build.append(getEncoding(x + dx, y + dy));
            }
        }
        return build.toString();
    }

    /**
     * Checks whether the given position is a valid position that the agent can
     * be in in this this. A position is valid if it is inside the bounds of the
     * maze and is not a tree (T)
     *
     * @param x The x position to check
     * @param y The y position to check.
     * @return True if the given (x, y) position is a valid position in the maze
     */
    protected boolean isValidPosition(int x, int y) {
        return !(x < 0 || y < 0 || y >= mazeTiles.length || x >= mazeTiles[0].length) && mazeTiles[y][x] != 'T' && mazeTiles[y][x] != 'N';
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getState() {
//        logger.info(String.format("x,y:%d %d", x, y));
        return getStringForState(x, y);
    }

    public abstract ActionPareto getReward(String state, int action);

    public boolean isEndOfProblem(String state) {
        for (Point finalState : finalStates) {
            if (getStringForState(finalState.x, finalState.y).equals(state)) {
                return true;
            }
        }
        return false;
    }

    private ArrayList<ArrayList<StepSnapshot>> traceReward(int exp_repeat, int timeStamp, NXCS nxcs,
                                                           NXCSParameters params, double[] weight, double first_reward) {
        // stats variables
        ArrayList<ArrayList<StepSnapshot>> locStats = new ArrayList<ArrayList<StepSnapshot>>();
        for (Point p : this.openLocations) {
            this.resetToSamePosition(p);
            String startState = this.getState();
            ArrayList<StepSnapshot> trc = GetTrainingPAResultInCSV(exp_repeat, timeStamp, nxcs, weight, first_reward, mp.method);
            // ArrayList<StepSnapshot> trc = trace.traceStart(startState, nxcs);
            trc.stream().forEach(x -> x.setTimestamp(timeStamp));
            locStats.add(trc);
        }
        return locStats;
    }

    public void printOpenLocationClassifiers(int timestamp, NXCS nxcs, List<double[]> weightList, double obj_r1, int method) {
        logger.error("R1 is:" + obj_r1 + " R2 is:" + (1000 - obj_r1));

        for (Point p : this.openLocations) {
            logger.error(String.format("%d\t location:%d,%d", timestamp, (int) p.getX(), (int) p.getY()));
            List<Classifier> C = nxcs.generateMatchSetAllweightNoDeletion(getStringForState((int) p.getX(), (int) p.getY()), this.getCurrentLocation(), method);
            for (double[] weight : weightList) {
                logger.error("weight0:" + weight[0] + " weight1:" + weight[1]);
                List<Classifier> A = C.stream().filter(b -> b.weight_moead == weight).collect(Collectors.toList());
                double[] PA1 = nxcs.generatePredictions(A, 0);
                for (int i = 0; i < PA1.length; i++) {
                    logger.error("PA1[" + i + "]:" + PA1[i]);
                }
                double[] PA2 = nxcs.generatePredictions(A, 1);
                for (int i = 0; i < PA2.length; i++) {
                    logger.error("PA2[" + i + "]:" + PA2[i]);
                }
                double[] PA = nxcs.generateTotalPredictions_Norm(A, weight);
                for (int i = 0; i < PA.length; i++) {
                    logger.error("PAt[" + i + "]:" + PA[i]);
                }
            }
            //left to f1, right to f2
            //Q_finalreward
//            double Q_finalreward_left = PA1[1];
//            double Q_finalreward_right = PA1[2];
//            double Q_finalreward_delta = PA1[1] - PA1[2];
//            double Q_finalreward_max = 0;
//            if (PA1[1] > PA1[2]) {
//                Q_finalreward_max = PA1[1];
//            } else {
//                Q_finalreward_max = PA1[2];
//            }
//
//            //Q_steps
//            double Q_steps_left = PA2[1];
//            double Q_steps_right = PA2[2];
//            double Q_steps_delta = PA2[1] - PA2[2];
//            double Q_steps_min = 0;
//            if (PA2[1] > PA2[2]) {
//                Q_steps_min = PA2[2];
//            } else {
//                Q_steps_min = PA2[1];
//            }

            //Q_weighted sum value for different weights
            //TODO:Q_weighted sum value for different weights


            for (int action : act) {

//                List<Classifier> A = C.stream().filter(b -> b.action == action && b.weight_moead[1] == 0).collect(Collectors.toList());
                List<Classifier> A = C.stream().filter(b -> b.action == action).collect(Collectors.toList());
                Collections.sort(A, new Comparator<Classifier>() {
                    @Override
                    public int compare(Classifier o1, Classifier o2) {
                        return o1.fitness == o2.fitness ? 0 : (o1.fitness > o2.fitness ? 1 : -1);
                    }
                });

                logger.error("action:" + action);
                // TODO:
                // 1.why not print fitness of cl???????
                // 2.print PA for each state to see if PA correct
                logger.error(A);

            }

        } // open locations
    }


    public void generateCoveringClassifierbyWeight(List<Point> openLocations, List<double[]> weights, NXCSParameters params) {
//		assert (state != null && matchSet != null) : "Invalid parameters";
//		assert (state.length() == this.np.stateLength) : "Invalid state length";

        for (Point location : openLocations) {

            String state = getStringForState(location.x, location.y);

            for (int act = 0; act < 4; act++) {
                for (int w = 0; w < weights.size(); w++) {
                    Classifier clas = new Classifier(this.np, state);
//				Set<Integer> usedActions = matchSet.stream().map(c -> c.action).distinct().collect(Collectors.toSet());
//				Set<Integer> unusedActions = IntStream.range(0, this.np.numActions).filter(i -> !usedActions.contains(i)).boxed()
//						.collect(Collectors.toSet());
                    clas.action = act;
                    clas.timestamp = 0;//TODO: timestamp;
                    clas.setWeight_moead(weights.get(w));
                }
            }
        }

    }

//    public ArrayList<StepSnapshot> GetTestingPAResultInCSV(int experiment_num, int timestamp, NXCS nxcs, double[] weight, double obj_r1, int[] ActionSelect) {
//
//        ArrayList<StepSnapshot> PAresult = new ArrayList<StepSnapshot>();
//
//
//        for (int p = 0; p < this.openLocations.size(); p++) {
//
//            Point point = new Point(p + 2, 1);
//
//            List<Classifier> C = nxcs.generateMatchSetAllweight(this.getStringForState(this.openLocations.get(p).x, this.openLocations.get(p).y));
//            double[] PA1 = nxcs.generatePredictions(C, 0);
//
//            double[] PA2 = nxcs.generatePredictions(C, 1);
//
//            double[] PA1_nor = new double[4];
//            double[] PA2_nor = new double[4];
//
//            //normalisation
//            for (int i = 0; i < PA1.length; i++) {
//                PA1_nor[i] = nxcs.stepNor(PA1[i], 100);
//            }
//            for (int i = 0; i < PA2.length; i++) {
//                PA2_nor[i] = nxcs.rewardNor(PA2[i], 1000, 0);
//            }
//
//            double[] PAt = nxcs.getTotalPrediciton(weight, PA1_nor, PA2_nor);
//
//            //Q_finalreward
//            double Q_finalreward_left = PA1[1];
//            double Q_finalreward_right = PA1[2];
//            double Q_finalreward_delta = PA1[1] - PA1[2];
//            double Q_finalreward_max = 0;
//            if (PA1[1] > PA1[2]) {
//                Q_finalreward_max = PA1[1];
//            } else {
//                Q_finalreward_max = PA1[2];
//            }
//
//
//            //Q_steps
//            double Q_steps_left = PA2[1];
//            double Q_steps_right = PA2[2];
//            double Q_steps_delta = PA2[1] - PA2[2];
//            double Q_steps_min = 0;
//            if (PA2[1] > PA2[2]) {
//                Q_steps_min = PA2[2];
//            } else {
//                Q_steps_min = PA2[1];
//            }
//
//            double Q_total_left = PAt[1];
//            double Q_total_right = PAt[2];
//
//
//            double Q_finalreward_select = PA1[ActionSelect[p]];
//            double Q_steps_select = PA2[ActionSelect[p]];
//            double Q_total_select = PAt[ActionSelect[p]];
//
//            //int exp_repeat, int finalCount, Point openState, double Q_finalreward_left, double Q_finalreward_right,double Q_finalreward_delta,double Q_finalreward_max, double Q_steps_left, double Q_steps_right,double Q_steps_delta,double Q_steps_max,ArrayList<Point> path) {
//
//            StepSnapshot result_row = new StepSnapshot(null, null, 0);//experiment_num, timestamp, weight, obj_r1, point, Q_finalreward_left, Q_finalreward_right, Q_finalreward_delta, Q_finalreward_max, Q_steps_left, Q_steps_right, Q_steps_delta, Q_steps_min, Q_total_left, Q_total_right, Q_finalreward_select, Q_steps_select, Q_total_select);
//
//            PAresult.add(result_row);
//            //Q_weighted sum value for different weights
//            //TODO:Q_weighted sum value for different weights
//
//
//        } // open locations
//        return PAresult;
//    }

    public ArrayList<StepSnapshot> GetTrainingPAResultInCSV(int experiment_num, int timestamp, NXCS nxcs, double[] weight, double obj_r1, int method) {

        ArrayList<StepSnapshot> PAresult = new ArrayList<StepSnapshot>();


        for (Point p : this.openLocations) {

            List<Classifier> C = nxcs.generateMatchSetAllweightNoDeletion(this.getStringForState(p.x, p.y), this.getCurrentLocation(), method);
            double[] PA1 = nxcs.generatePredictions(C, 0);

            double[] PA2 = nxcs.generatePredictions(C, 1);

            //Q_finalreward
            double Q_finalreward_left = PA1[1];
            double Q_finalreward_right = PA1[2];
            double Q_finalreward_delta = PA1[1] - PA1[2];
            double Q_finalreward_max = 0;
            if (PA1[1] > PA1[2]) {
                Q_finalreward_max = PA1[1];
            } else {
                Q_finalreward_max = PA1[2];
            }

            //Q_steps
            double Q_steps_left = PA2[1];
            double Q_steps_right = PA2[2];
            double Q_steps_delta = PA2[1] - PA2[2];
            double Q_steps_max = 0;
            if (PA2[1] > PA2[2]) {
                Q_steps_max = PA2[1];
            } else {
                Q_steps_max = PA2[2];
            }

            //int exp_repeat, int finalCount, Point openState, double Q_finalreward_left, double Q_finalreward_right,double Q_finalreward_delta,double Q_finalreward_max, double Q_steps_left, double Q_steps_right,double Q_steps_delta,double Q_steps_max,ArrayList<Point> path) {

            StepSnapshot result_row = new StepSnapshot(null, null, 0);//experiment_num, timestamp, weight, obj_r1, p, Q_finalreward_left, Q_finalreward_right, Q_finalreward_delta, Q_finalreward_max, Q_steps_left, Q_steps_right, Q_steps_delta, Q_steps_max);

            PAresult.add(result_row);
            //Q_weighted sum value for different weights
            //TODO:Q_weighted sum value for different weights


        } // open locations
        return PAresult;
    }


    public ArrayList<ArrayList<StepSnapshot>> getOpenLocationExpectPaths() {
        ArrayList<ArrayList<StepSnapshot>> expect = new ArrayList<ArrayList<StepSnapshot>>();
        ArrayList<StepSnapshot> e21 = new ArrayList<StepSnapshot>();
        e21.add(new StepSnapshot(new Point(2, 1), new Point(1, 1), 1));
        e21.add(new StepSnapshot(new Point(2, 1), new Point(6, 1), 4));
        expect.add(e21);
        ArrayList<StepSnapshot> e31 = new ArrayList<StepSnapshot>();
        e31.add(new StepSnapshot(new Point(3, 1), new Point(1, 1), 2));
        e31.add(new StepSnapshot(new Point(3, 1), new Point(6, 1), 3));
        expect.add(e31);
        ArrayList<StepSnapshot> e41 = new ArrayList<StepSnapshot>();
        e41.add(new StepSnapshot(new Point(4, 1), new Point(6, 1), 2));
        expect.add(e41);
        ArrayList<StepSnapshot> e51 = new ArrayList<StepSnapshot>();
        e51.add(new StepSnapshot(new Point(5, 1), new Point(6, 1), 1));
        expect.add(e51);

        return expect;
    }

    public MazeBase initialize(MazeParameters mp, NXCSParameters np, ArrayList<Hashtable<Point, ActionPareto>> positionRewards, HyperVolumn hyperVolumnCalculator, IParetoCalculator paretoCalculator) throws IOException {
        logger.info("\n\n=========================   " + this.getClass().getName() + "   ========================================");

        this.mp = mp;
        this.np = np;
        this.positionRewards = positionRewards;

        // Set up the encoding table FOR DST
        encodingTable = new HashMap<Character, String>();
        encodingTable.put('O', "000");
        encodingTable.put('T', "110");
        encodingTable.put(null, "100");// For out of the maze positions
        encodingTable.put('F', "111");
        encodingTable.put('N', "011");
        encodingTable.put('M', "001");

        // encodingTable.put('1', "001");
        // encodingTable.put('3', "011");
        // encodingTable.put('5', "101");
        // encodingTable.put('8', "010");

        openLocations = new ArrayList<Point>();
        finalStates = new ArrayList<Point>();
        rewardGrid = new ArrayList<Reward>();

        act = new ArrayList<>();
        act.add(0);
        act.add(1);
        act.add(2);
        act.add(3);

        actions = new ArrayList<Point>();
        actions.add(new Point(0, -1));// Up
        actions.add(new Point(-1, 0));// Left
        actions.add(new Point(1, 0));// Right
        actions.add(new Point(0, 1));// Down
        // actions.add(new Point(-1, -1));// Up, Left
        // actions.add(new Point(1, -1));// Up, Right
        // actions.add(new Point(-1, 1));// Down, Left
        // actions.add(new Point(1, 1));// Down, Right

        // Load the maze into a char array
        List<String> mazeLines = new ArrayList<String>();
        try (BufferedReader reader = new BufferedReader(new FileReader(this.mazeFile))) {
            String line;
            while ((line = reader.readLine()) != null) {
                mazeLines.add(line);
            }
        } catch (Exception e) {
            throw new IOException(String.format("Failed to load file:%s", this.mazeFile));
        }
        mazeTiles = new char[mazeLines.size()][];
        for (int i = 0; i < mazeLines.size(); i++) {
            mazeTiles[i] = mazeLines.get(i).toCharArray();
            if (i > 0 && mazeTiles[i].length != mazeTiles[1].length) {
                throw new IllegalArgumentException(
                        String.format("Line %d in file %s is of different length than the others", i + 1, this.mazeFile.getName()));
            }

            for (int j = 0; j < mazeTiles[i].length; j++) {
                char c = mazeTiles[i][j];
                if (!encodingTable.containsKey(c)) {
                    throw new IllegalArgumentException(
                            String.format("Line %d in file %s has an invalid character %c", i + 1, this.mazeFile.getName(), c));
                }

                if (c == 'O') {
                    openLocations.add(new Point(j, i));
                    rewardGrid.add(new Reward(new Point(j, i), -1, 0));
//				} else if (c != 'O' && c != 'T') {
                } else if (c == 'F') {
                    finalStates.add(new Point(j, i));
                    rewardGrid.add(new Reward(new Point(j, i), -1, Character.getNumericValue(c)));
                }
            }

        }


        this.hyperVolumnCalculator = hyperVolumnCalculator;
        this.paretoCalculator = paretoCalculator;

//        logger.debug("rewardGrid:" + rewardGrid);
//        logger.info("rewards:" + this.positionRewards);
        logger.error("===========Maze Parameters============\t" + gson.toJson(this.mp));
        logger.error("===========NXCS Parameters============\t" + gson.toJson(this.np));
        logger.error("===========Open Locations=============\t:" + openLocations);
        logger.error("===========Final States===============\t:" + finalStates);
        logger.error("===========Position Rewards===========\t" + gson.toJson(this.positionRewards));

        if (mp.method != 0) {
            if (checkOpenLocationDuplicateEncoding()) {
                throw new IOException("FATAL Error: duplicate open locations!");
            }
        }
        return this;
    }

    public Point getCurrentLocation() {
        return new Point(x, y);
    }

    public ArrayList<Point> getOpenLocations() {
        return this.openLocations;
    }

    /***
     * move to next valid location
     * @param action
     */
    public void move(int action) {
        stepCount++;
        Point movement = actions.get(action);
        if (isValidPosition(x + movement.x, y + movement.y)) {
            x += movement.x;
            y += movement.y;
        }
    }

    public boolean checkOpenLocationDuplicateEncoding() {
        boolean dup = false;
        Hashtable<String, Point> locs = new Hashtable<String, Point>();
        ArrayList<Point> duplocs = new ArrayList<Point>();
        HashSet<String> codes = new HashSet<>();
        for (Point p : this.openLocations) {
            try {
                int x, y;
                x = p.y;
                y = p.x;
                String code = "";
                code += String.valueOf((mazeTiles[x - 1][y - 1]));
                code += String.valueOf((mazeTiles[x][y - 1]));
                code += String.valueOf((mazeTiles[x + 1][y - 1]));
                code += String.valueOf((mazeTiles[x - 1][y]));
                code += String.valueOf((mazeTiles[x][y]));
                code += String.valueOf((mazeTiles[x + 1][y]));
                code += String.valueOf((mazeTiles[x - 1][y + 1]));
                code += String.valueOf((mazeTiles[x][y + 1]));
                code += String.valueOf((mazeTiles[x + 1][y + 1]));

                //logger.debug("Code:" + code);
                if (codes.contains(code)) {
                    if (!duplocs.contains(p))
                        duplocs.add(locs.get(code));
                    duplocs.add(p);
                    dup = true;
                } else {
                    locs.put(code, p);
                    codes.add(code);
                }
            } catch (Exception e) {
                logger.debug(p.toString());
            }
        }

        logger.debug(codes);

        return dup;
    }

    public ArrayList<StepSnapshot> trace(MOEAD moeadObj, NXCS nxcs, StepStatsLogger stepStatsLogger, int trailIndex, double[] targetWeight, double objective, int timestamp, int method) throws Exception {
        ArrayList<StepSnapshot> testStats = new ArrayList<StepSnapshot>();


        for (double[] traceMoeadWeight : this.getTraceWeight(moeadObj.weights)) {

            ArrayList<StepSnapshot> weightStats = new ArrayList<StepSnapshot>();
            double hyperVolumnSum = 0;

            logger.debug(String.format("Test on  weight: %f, %f ", traceMoeadWeight[0], traceMoeadWeight[1]));


            int resetPoint = 0;

            for (Point openState : this.openLocations) {
                this.resetToSamePosition(openState);
                List<Point> path = new ArrayList<>();
                path.add(openState);
                double[] PA1 = new double[]{0, 0, 0, 0};
                double[] PA2 = new double[]{0, 0, 0, 0};
                double[] PA = new double[]{0, 0, 0, 0};
//                if (this.finalStateCount >= 1000) {
//                    List<Classifier> C = nxcs.generateMatchSetAllweightNoDeletion(this.getStringForState(openState.x, openState.y));
//                    PA1 = nxcs.generatePredictions(C, 0);
//                    PA2 = nxcs.generatePredictions(C, 1);
//                    PA = nxcs.generateTotalPredictions_Norm(C, traceMoeadWeight);
//                    statLogger.info(String.format("tracePA:%f-%f,%f,%f,%f,%f", PA[0], PA[1], PA[2], PA[3]));
//                }

                while (!this.isEndOfProblem(this.getState())) {
                    String state = this.getState();
//                    if (stepCount > 20) {
//                        nxcs.getMatchsetFromClassifier(nxcs.matchSet(state, traceMoeadWeight).get(0));
//                    }
                    int action = nxcs.classify(state, this.getCurrentLocation(), traceMoeadWeight, method);
                    logger.debug(String.format("@1 Test:%d, Steps:%d, state:%s, action:%d", resetPoint, this.stepCount, this.getCurrentLocation(), action));

                    //TODO:return the PA1[action]
                    //logger.info(String.format("@2 Timestamp:%d, test:%d, resetPoint:%d, logFlag:%d, state:%s", timestamp, test, resetPoint, logFlag, this.getCurrentLocation()));

                    //double selectedPA_reward = nxcs.getSelectPA(action, state);
                    //ActionPareto r = this.getReward(state, action, first_Freward);

                    this.move(action);
                    path.add(this.getCurrentLocation());
//                    logger.info(String.format("trace classify,%s,%f,%f,%f,%f,%f,%f,%d", this.getCurrentLocation(), traceMoeadWeight[0], traceMoeadWeight[1], nxcs.PAtotal[0], nxcs.PAtotal[1], nxcs.PAtotal[2], nxcs.PAtotal[3], action));

                    if (this.isEndOfProblem(this.getState())) {
                        hyperVolumnSum += getHyperVolumn(getParetoByState(nxcs, openState, moeadObj.getWeights()));
                        //if path>100(step>100) means fail to reach the final state
                        double[] pa0 = new double[nxcs.PA0.length];
                        double[] pa1 = new double[nxcs.PA0.length];
                        double[] paTotal = new double[nxcs.PA0.length];
                        for (int i = 0; i < nxcs.PA0.length; i++) {
                            pa0[i] = nxcs.PA0[i];
                        }
                        for (int i = 0; i < nxcs.PA0.length; i++) {
                            pa1[i] = nxcs.PA1[i];
                        }
                        for (int i = 0; i < nxcs.PA0.length; i++) {
                            paTotal[i] = nxcs.PAtotal[i];
                        }
                        StepSnapshot row = new StepSnapshot(trailIndex, timestamp, openState, this.getCurrentLocation(), targetWeight
                                , objective, traceMoeadWeight, this.stepCount, 0, path
                                , pa0, pa1, paTotal);
                        //TODO: collect stats, trailIndex, finalState(timestamp), targetWeight, traceWeight, OpenState, FinalState, steps, hpyerVolumn
                        weightStats.add(row);
                        logger.info(String.format("@3 Test:%s, Steps:%d, to state:%s", openState, this.stepCount, this.getCurrentLocation()));
                        //logger.info(String.format("##Collectd row:\t%s", row.to_Total_CSV_PA()));
                    }
                }
            }
            final double hypersum = hyperVolumnSum;
            weightStats.stream().forEach(x -> x.setHyperVolumn(hypersum));
            testStats.addAll(weightStats);
            stepStatsLogger.add(weightStats);

            logger.info(String.format("End of trail:%d, %d/%d,  weight: %f, %f", trailIndex, finalStateCount, this.mp.finalStateUpperBound, traceMoeadWeight[0], traceMoeadWeight[1]));
        }//loop test weight

//        this.printOpenLocationClassifiers(this.finalStateCount, nxcs, this.getTraceWeight(moeadObj.weights), 0);

        return testStats;
    }


    private ArrayList<ActionPareto> getParetoByState(NXCS nxcs, Point location, List<double[]> weights) throws Exception {
        ArrayList<ActionPareto> ret = new ArrayList<ActionPareto>();
        //UPDATE!!!!!!!FIX
//        List<Classifier> C = nxcs.generateMatchSetAllweightNoDeletion(this.getStringForState(location.x, location.y), this.getPoint(), mp.method);
        List<Classifier> C = nxcs.generateMatchSetAllweightNoDeletion(this.getStringForState(location.x, location.y), location, mp.method);

        for (double[] w : weights) {
            for (int a : this.act) {
                List<Classifier> Cweight = C.stream().filter(x -> Arrays.equals(x.weight_moead, w) && x.action == a).collect(Collectors.toList());
                if (Cweight.size() > 1) {
                    logger.debug("more than one classifier in this weight + action");
                    //TODO: sort by fitness and retain the one with highest fitness
                    Collections.sort(Cweight, new Comparator<Classifier>() {
                        @Override
                        public int compare(Classifier o1, Classifier o2) {
                            return o1.fitness == o2.fitness ? 0 : (o1.fitness > o2.fitness ? -1 : 1);
                        }
                    });
//                    throw new Exception("more then one classifier in this weight + action");
                }
                if (Cweight.size() > 0) {
                    try {
                        ret.add(new ActionPareto(new Qvector(Cweight.get(0).prediction[0], Cweight.get(0).prediction[1]), 0));
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }
            }
        }
        return ret;
    }

    private double getHyperVolumn(List<ActionPareto> paretos) {
        return paretos.size() == 0 ? 0 : hyperVolumnCalculator.calcHyperVolumn(paretoCalculator.getPareto(paretos), new Qvector(-10, -10));
    }

    public boolean isTraceConditionMeet() {
        return (this.finalStateCount % this.mp.resultInterval == 0);
    }

    public List<double[]> getTraceWeight(List<double[]> traceWeights) {
        return traceWeights;
    }

    public int getCurrentFinalStateCount() {
        return this.finalStateCount;
    }

    public List<Integer> getActions() {
        return this.act;
    }
}