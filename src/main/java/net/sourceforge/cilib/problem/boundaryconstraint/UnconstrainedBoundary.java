/*
 * Copyright (C) 2003 - 2008
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
package net.sourceforge.cilib.problem.boundaryconstraint;

import net.sourceforge.cilib.entity.Entity;

/**
 * This is the default boundary constraint. This constraint defines
 * that no boundary checking is to be performed.
 * @author gpampara
 */
public class UnconstrainedBoundary implements BoundaryConstraint {

    private static final long serialVersionUID = -6672863576480662484L;

    /**
     * {@inheritDoc}
     */
    @Override
    public UnconstrainedBoundary getClone() {
        return this;
    }


    /**
     * This enforcement of the boundary constraint does nothing.
     * @param entity The entity to which no contraint is to be applied.
     */
    @Override
    public void enforce(Entity entity) {
        // Do nothing as there is no boundary constraint to enforce
    }

}
