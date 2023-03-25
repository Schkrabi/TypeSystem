package velka.test;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.util.Arrays;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import velka.util.AppendableException;
import velka.util.ClojureHelper;
import velka.util.ClojureHelper.ProxyImpl;

class TestClojureHelper extends VelkaTest {

	@BeforeAll
	static void setUpBeforeClass() throws Exception {
	}

	@AfterAll
	static void tearDownAfterClass() throws Exception {
	}

	@BeforeEach
	void setUp() throws Exception {
	}

	@AfterEach
	void tearDown() throws Exception {
	}

	@Test
	void testProxy() throws NoSuchMethodException, SecurityException, IOException, InterruptedException, AppendableException {
		String code = ClojureHelper.proxy(
					java.util.Comparator.class,
					Arrays.asList(),
					ProxyImpl.of(java.util.Comparator.class.getDeclaredMethod("compare", Object.class, Object.class),
							Arrays.asList("a b"),
							ClojureHelper.applyClojureFunction(
									"-",
									ClojureHelper.applyClojureFunction(
											"compare", "a", "b"))));
		
		String rslt = this.clojureCodeResult(
				ClojureHelper.applyClojureFunction(
						"print",
						ClojureHelper.applyClojureFunction(
							".compare",
							code,
							Integer.toString(42),
							Integer.toString(21))));
		
		assertEquals(Integer.toString(-1), rslt);
	}
	
	@Test
	void testConstructJavaClass() throws Exception {
		String code = ClojureHelper.constructJavaClass(
				java.util.ArrayList.class,
				ClojureHelper.clojureVectorHelper(
						Integer.toString(1), Integer.toString(2), Integer.toString(3)));
		
		String rslt = this.clojureCodeResult(
				ClojureHelper.applyClojureFunction(
						"print",
						ClojureHelper.applyClojureFunction(
								".toString",
								code)));
		
		assertEquals(Arrays.asList(1, 2, 3).toString(), rslt);
	}

}
