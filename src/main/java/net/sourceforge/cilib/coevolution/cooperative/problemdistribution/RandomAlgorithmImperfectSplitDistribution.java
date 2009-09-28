/**
 * Copyright (C) 2003 - 2009
 * Computational Intelligence Research Group (CIRG@UP)
 * Department of Computer Science
 * University of Pretoria
 * South Africa
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
/**
 *
 */
package net.sourceforge.cilib.coevolution.cooperative.problemdistribution;

import java.util.List;

import net.sourceforge.cilib.algorithm.Algorithm;
import net.sourceforge.cilib.algorithm.InitialisationException;
import net.sourceforge.cilib.algorithm.iterator.RandomAlgorithmIterator;
import net.sourceforge.cilib.algorithm.population.PopulationBasedAlgorithm;
import net.sourceforge.cilib.coevolution.cooperative.CooperativeCoevolutionAlgorithm;
import net.sourceforge.cilib.coevolution.cooperative.problem.CooperativeCoevolutionProblemAdapter;
import net.sourceforge.cilib.coevolution.cooperative.problem.DimensionAllocation;
import net.sourceforge.cilib.coevolution.cooperative.problem.SequencialDimensionAllocation;
import net.sourceforge.cilib.problem.OptimisationProblem;
import net.sourceforge.cilib.type.types.container.Vector;

/**
 * This {@linkplain ProblemDistributionStrategy} performs a split by assining a sequencial portion of the varying length to each participating {@linkplain PopulationBasedAlgorithm}. Defaults into a
 * split of equal sizes if possible.
 * The order in which the algorithms are assigned is generated randomly.
 * @author leo
 */
public class RandomAlgorithmImperfectSplitDistribution implements
        ProblemDistributionStrategy {

    /**
     * Splits up the given {@link OptimisationProblem} into sub-problems, where each sub problem contains a sequencial, non-uniform length, portion of the problem verctor, and assigns all the sub-problems to the sub population {@link Algorithm}s.
     * This implimentation assigns a portion of length dimensionality/number of populations + 1 to dimensionality % number of populations of the participating poopulations.
     * The order in which the algorithms are assigned is generated randomly.
     * @param populations The list of participating {@linkplain PopulationBasedAlgorithm}s.
     * @param problem The problem that needs to be re-distributed.
     * @param context The context vector maintained by the {@linkplain CooperativeCoevolutionAlgorithm}.
     */
    public void performDistribution(List<PopulationBasedAlgorithm> populations,
            OptimisationProblem problem, Vector context) {
        if (populations.size() < 2)
            throw new InitialisationException("There should at least be two Cooperating populations in a Cooperative Algorithm");

        int dimension = problem.getDomain().getDimension() / populations.size();
        int oddDimensions = problem.getDomain().getDimension() % populations.size();
        int i = 0;
        int offset = 0;
        RandomAlgorithmIterator<PopulationBasedAlgorithm> iterator = new RandomAlgorithmIterator<PopulationBasedAlgorithm>(populations);
        while (iterator.hasNext()) {
            int actualDimension = dimension;
            if(i < oddDimensions)
                actualDimension++;

            DimensionAllocation problemAllocation = new SequencialDimensionAllocation(offset, actualDimension);
            iterator.next().setOptimisationProblem(new CooperativeCoevolutionProblemAdapter(problem, problemAllocation, context));
            offset += actualDimension;

            ++i;
        }
    }

}
