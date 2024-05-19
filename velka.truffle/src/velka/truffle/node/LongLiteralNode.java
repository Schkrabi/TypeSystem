package velka.truffle.node;

import java.util.Objects;

import com.oracle.truffle.api.frame.VirtualFrame;

public class LongLiteralNode extends VelkaNode {
	private final long value;

    public LongLiteralNode(long value) {
        this.value = value;
    }
    
    @Override
    public Object executeGeneric(VirtualFrame virtualFrame) {
        return value;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        LongLiteralNode that = (LongLiteralNode) o;
        return value == that.value;
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }

    public long getValue() {
        return value;
    }

    @Override
    public String toString() {
        return String.valueOf(value);
    }
}
