import { DatabaseServices } from "services/DatabaseServices"

module.exports = async () => {
    console.log(`Checking database structure ...`)

    // Checks database integrity
    await DatabaseServices.CheckAvailableData()

    console.log(`Database structure check done.`)
}