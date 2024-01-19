```groovy

import java.util.concurrent.TimeUnit;

import static com.google.common.collect.Maps.newHashMap
import static com.google.common.collect.Sets.newHashSet
import static com.google.common.util.concurrent.Uninterruptibles.sleepUninterruptibly

/**
 * A service which expects to receive a jar file containing a class which implements the
 * {@code FunctionalInterface} and which has an empty constructor. The service will instantiate
 * the class from the jar, create a wrapper for it and cache the reference for future use.
 */
class FastPassService {

    private final ClassLoader classLoader
    private final Map<String, FunctionalInterface> cache = newHashMap()
    private final Set<Future<FunctionalInterface>> pending = newHashSet()

    /**
     * @param classLoader the class loader to use to load the class from the jar
     */
    public FastPassService(ClassLoader classLoader) {
        this.classLoader = classLoader
    }

    /**
     * @param jarFile the jar file containing the implementation class
     * @return a consistent instance of the implementation class
     * @throws IllegalArgumentException if the {@code jarFile} does not contain a valid implementation
     * class or the class is not correctly implemented
     */
    public FunctionalInterface get(File jarFile) {
        if (!cache.containsKey(jarKey(jarFile))) {
            Future<FunctionalInterface> future = submit(jarFile)
            try {
                pending.add(future)
                cache.put(jarKey(jarFile), future.get(1, TimeUnit.DAYS))
            } catch (Exception e) {
                cache.remove(jarKey(jarFile), future)
                throw new IllegalArgumentException("invalid jar file", e)
            } finally {
                pending.remove(future)
            }
        }
        return cache.get(jarKey(jarFile))
    }

    /**
     * Attempts to load the class from the {@code jarFile} asynchronously.
     * @param jarFile the jar file containing the implementation class
     * @return a future reference to the implementation class
     */
    private Future<FunctionalInterface> submit(File jarFile) {
        class AsyncLoad implements Callable<FunctionalInterface> {
            final File file

            AsyncLoad(File file) {
                this.file = file
            }

            @Override
            FunctionalInterface call() {
                JarURLConnection jarURLConnection = new URL(
                        "jar", "", file.toURI().toURL().toString() + "!/"
                ).openConnection()
                jarURLConnection.connect()

                JarFile jarFile = jarURLConnection.getJarFile()
                Enumeration<JarEntry> entries = jarFile.entries()

                String className = ''
                while (entries.hasMoreElements()) {
                    JarEntry entry = entries.nextElement()
                    if (entry.name.endsWith('.class') &&
                            !entry.name.contains('$') &&
                            !className.isEmpty()) {
                        throw new IllegalStateException("multiple classes with empty constructor found")
                    }
                    if (entry.name.endsWith('.class') &&
                            !className.isEmpty() &&
                            FunctionalInterface.class.name.replace('.', '/') == className.replace('$', '/')) {
                        throw new IllegalStateException("there cannot be more than one class per jar")
                    }
                    if (entry.name.endsWith('.class') && className.isEmpty() &&
                            FunctionalInterface.class.name.replace('.', '/') == entry.name.replace('$', '/')) {
                        className = entry.name.substring(0, entry.name.length() - '.class'.length())
                    }
                }

                URLClassLoader urlClassLoader = new URLClassLoader(new URL[]{jarFile.url}, classLoader)
                return create(urlClassLoader, className)
            }
        }
        return Executors.newSingleThreadExecutor().submit(new AsyncLoad(jarFile))
    }

    /**
     * Creates an instance of the class with the specified {@code className} from the
     * {@code classLoader} ensuring that the constructor is empty.
     * @param classLoader the class loader to use
     * @param className the name of the class in the jar file
     * @return a new instance of the implementation class
     */
    private FunctionalInterface create(URLClassLoader classLoader, String className) {
        try {
            Class<?> clazz = classLoader.loadClass(className)
            if (clazz.getConstructor().getParameterTypes().length > 0) {
                throw new IllegalStateException("invalid constructor found")
            }
            return (FunctionalInterface) clazz.newInstance()
        } catch (Exception e) {
            throw new IllegalArgumentException("invalid jar file", e)
        }
    }

    /**
     * Computes a stable key for the specified {@code jarFile}.
     * @param jarFile the jar file
     */
    private String jarKey(File jarFile) {
        return jarFile.toURI().toURL().normalize().toString()
    }

    /**
     * Waits until all pending requests are complete.
     * @param timeout the maximum time to wait for any pending request to complete, or null to wait
     * indefinitely
     * @param unit the time unit of the {@code timeout} argument
     * @return {@code true} if all pending requests are completed
     */
    public boolean waitForPending(Long timeout, TimeUnit unit) {
        if (timeout != null) {
            sleepUninterruptibly(timeout, unit)
        }
        return pending.isEmpty()
    }
}

// wait for all pending operations in the fast-pass service to complete before exiting
addShutdownHook {
    if (fastPassService.waitForPending(2, TimeUnit.MINUTES)) {
        println "shutting down successfully"
    } else {
        println "some requests did not complete in time, exiting anyway"
    }
}

```