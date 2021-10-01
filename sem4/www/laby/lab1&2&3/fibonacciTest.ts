import { fibonacci } from "./fibonacci";
import { expect } from "chai";
import "mocha";

describe("Fibonacci", () => {
  it("should equal 0 for call with 0", () => {
    expect(fibonacci(0)).to.equal(0);
  });
});

describe("Fibonacci", () => {
  it("should equal 1 for call with 1", () => {
    expect(fibonacci(1)).to.equal(1);
  });
});

describe("Fibonacci", () => {
  it("should equal 5 for call with 5", () => {
    expect(fibonacci(5)).to.equal(5);
  });
});

describe("Fibonacci", () => {
  it("should equal 55 for call with 10", () => {
    expect(fibonacci(10)).to.equal(55);
  });
});

describe("Fibonacci", () => {
  it("should equal 2584 for call with 18", () => {
    expect(fibonacci(18)).to.equal(2584);
  });
});
