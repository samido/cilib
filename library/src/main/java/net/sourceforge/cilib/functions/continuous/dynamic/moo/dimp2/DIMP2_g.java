/**           __  __
 *    _____ _/ /_/ /_    Computational Intelligence Library (CIlib)
 *   / ___/ / / / __ \   (c) CIRG @ UP
 *  / /__/ / / / /_/ /   http://cilib.net
 *  \___/_/_/_/_.___/
 */
package net.sourceforge.cilib.functions.continuous.dynamic.moo.dimp2;

import net.sourceforge.cilib.algorithm.AbstractAlgorithm;
import net.sourceforge.cilib.functions.ContinuousFunction;
import net.sourceforge.cilib.type.types.container.Vector;

/**
 *  This function is the g function of the DIMP2 problem defined in the following paper:
 * W.T. Koo and C.K. Goh and K.C. Tan. A predictive gradien strategy for multiobjective
 * evolutionary algorithms in a fast changing environment, Memetic Computing, 2:87-110,
 * 2010.
 *
 */

public class DIMP2_g implements ContinuousFunction {

    //members
    //number of generations for which t remains fixed
    private int tau_t;
    //generation counter
    private int tau;
    //number of distinct steps in t
    private int n_t;

    /**
     * Creates a new instance of FDA1_g.
     */
    public DIMP2_g() {
        //initialize the members
        this.tau_t =  5;
        this.tau = 1;
        this.n_t = 10;
    }

    /**
     * Sets the iteration number.
     * @param tau Iteration number.
     */
    public void setTau(int tau) {
        this.tau = tau;
    }

    /**
     * Returns the iteration number.
     * @return tau Iteration number.
     */
    public int getTau() {
        return this.tau;
    }

    /**
     * Sets the frequency of change.
     * @param tau_t Change frequency.
     */
    public void setTau_t(int tau_t) {
        this.tau_t = tau_t;
    }

    /**
     * Returns the frequency of change.
     * @return tau_t Change frequency.
     */
    public int getTau_t() {
        return this.tau_t;
    }

    /**
     * Sets the severity of change.
     * @param n_t Change severity.
     */
    public void setN_t(int n_t) {
        this.n_t = n_t;
    }

    /**
     * Returns the severity of change.
     * @return n_t Change severity.
     */
    public int getN_t() {
        return this.n_t;
    }

    /**
     * Evaluates the function.
     */
    @Override
    public Double apply(Vector x) {
        this.tau = AbstractAlgorithm.get().getIterations();
    	return this.apply(this.tau, x);
    }

    /**
     * Evaluates the function for a specific iteration.
     */
    public Double apply(int iteration, Vector x) {
        double t = (1.0/(double)n_t)*Math.floor((double)iteration/(double)this.tau_t);

        double sum = 1.0 + 2.0*(x.size());
        for (int k=0; k < x.size(); k++) {
            double G = Math.sin(0.5*Math.PI*t + 2.0*Math.PI*Math.pow((double)k/(double)(x.size()+2.0), 2));
            sum += Math.pow(x.doubleValueOf(k) - G, 2) - 2.0*Math.cos(3.0*Math.PI*(x.doubleValueOf(k)-G));
        }
        return sum;
    }
}
