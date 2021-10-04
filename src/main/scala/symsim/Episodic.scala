package symsim

trait Episodic:
   /**
    * Defines an upperbound on epochs, but it is not used for
    * learning, but only for testing (to detect timeouts).
    * For learning you should embed time into the state of the agent;
    * At least for now.
    */
   def TimeHorizon: Int

