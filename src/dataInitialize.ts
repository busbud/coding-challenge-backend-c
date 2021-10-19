import { DatabaseServices } from "services/DatabaseServices"

module.exports = async () => {
    console.log(`Checking database structure ...`)

    await DatabaseServices.CheckAvailableData()

    console.log(`Database structure check done.`)
}