using System;

/** \brief This is an arbitrary class acting as an Observer
*/
public class Observer {
    public int x;
    
    public Observer() {
        this.x = 5;
    }
    
    public void printNum() {
        Console.WriteLine(this.x);
    }
    
    public int getX() {
        return this.x;
    }
    
    public void setX(int x) {
        this.x = x;
    }
}
