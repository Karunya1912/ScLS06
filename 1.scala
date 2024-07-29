object InventoryManagement extends App {
  
  case class Product(id: Int, name: String, quantity: Int, price: Double)
  
  type Inventory = Map[Int, Product]
  
  //inventory 1
  val inventory1: Inventory = Map(
    101 -> Product(101, "ProductA", 10, 50.0),
    102 -> Product(102, "ProductB", 20, 30.0),
    103 -> Product(103, "ProductC", 15, 20.0)
  )

  //inventory 2
  val inventory2: Inventory = Map(
    102 -> Product(102, "ProductB", 5, 35.0),
    104 -> Product(104, "ProductD", 8, 25.0)
  )
  
  // I.All product from inventory1 
  def getAllProductNames(inventory: Inventory): Seq[String] = {
    inventory.values.map(_.name).toSeq
  }
  
  // II.Total value of all products in inventory1
  def calculateTotal(inventory: Inventory): Double = {
    inventory.values.map(product => product.quantity * product.price).sum
  }
  
  // III. Check is inventory1 empty
  def isInventoryEmpty(inventory: Inventory): Boolean = {
    inventory.isEmpty
  }
  
  // IV. Merge inventory1 and inventory2, updating quantities and retaining the highest price
  def mergeInventories(inv1: Inventory, inv2: Inventory): Inventory = {
    var mergedInventory = inv1
    for ((id, prod2) <- inv2) {
      if (mergedInventory.contains(id)) {
        val prod1 = mergedInventory(id)
        val updatedProduct = prod1.copy(
          quantity = prod1.quantity + prod2.quantity,
          price = math.max(prod1.price, prod2.price)
        )
        mergedInventory = mergedInventory + (id -> updatedProduct)
      } else {
        mergedInventory = mergedInventory + (id -> prod2)
      }
    }
    mergedInventory
  }
  
  // V. Check if a product with a specific ID exists and print its details
  def printDetails(inventory: Inventory, id: Int): Unit = {
    inventory.get(id) match {
      case Some(product) =>
        println(s"Product ID: ${product.id}, Name: ${product.name}, Quantity: ${product.quantity}, Price: ${product.price}")
      case None =>
        println(s"Product with ID $id does not exist in the inventory.")
    }
  }
  
  // Main functions
  println("All product names in inventory1:")
  println(getAllProductNames(inventory1))
  
  println("\nTotal value of all products in inventory1:")
  println(calculateTotal(inventory1))
  
  println("\nIs inventory1 empty?")
  println(isInventoryEmpty(inventory1))
  
  println("\nMerged inventory of inventory1 and inventory2:")
  val mergedInventory = mergeInventories(inventory1, inventory2)
  mergedInventory.foreach { case (_, product) => println(product) }
  
  println("\nCheck if product with ID 102 exists in inventory1 and print its details:")
  printDetails(inventory1, 102)
}
