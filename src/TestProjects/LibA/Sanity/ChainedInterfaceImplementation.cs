// ReSharper disable once CheckNamespace

namespace LibA.Sanity.ChainedInterfaceImplementation;

public interface I1
{ }

public interface I2 : I1
{ }

public interface I3 : I2
{ }

public class C : I2
{ }