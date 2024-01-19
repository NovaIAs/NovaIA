```typescript
// Importación de bibliotecas
import { Component, Input, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { ActivatedRoute, Router } from '@angular/router';
import { ToastrService } from 'ngx-toastr';
import { Cliente } from '../cliente';
import { ClienteService } from '../cliente.service';

@Component({
  selector: 'app-cliente-editar',
  templateUrl: './cliente-editar.component.html',
  styleUrls: ['./cliente-editar.component.css']
})

export class ClienteEditarComponent implements OnInit {
  // Decoradores
  @Input() cliente: Cliente;
  formCliente: FormGroup;

  // Constructor
  constructor(
    private formBuilder: FormBuilder,
    private clienteService: ClienteService,
    private activatedRoute: ActivatedRoute,
    private router: Router,
    private toastr: ToastrService
  ) { }

  ngOnInit() {
    this.formCliente = this.formBuilder.group({
      id: [this.cliente.id],
      nombre: [this.cliente.nombre, Validators.required],
      apellido: [this.cliente.apellido, Validators.required],
      telefono: [this.cliente.telefono, Validators.required],
      email: [this.cliente.email, Validators.required],
      direccion: [this.cliente.direccion, Validators.required]
    });
  }

  // Método para enviar el formulario
  onSubmit() {
    if (this.formCliente.invalid) {
      return;
    }
    const clienteEditar = this.formCliente.value;
    this.clienteService.editarCliente(clienteEditar).subscribe(
      (data) => {
        this.toastr.success('Cliente editado con éxito');
        this.router.navigate(['/clientes']);
      },
      (error) => {
        this.toastr.error('Error al editar el cliente');
      }
    );
  }
}
```

**Explicación:**

Este es un código complejo escrito en TypeScript que utiliza Angular para crear un formulario para editar un cliente en un sistema de gestión. El código está bien estructurado y utiliza buenas prácticas de programación, como la inyección de dependencias y la modularización.

El código se compone de dos componentes:

* `ClienteEditarComponent`: Este componente es el principal y se encarga de mostrar el formulario de edición del cliente.
* `ClienteService`: Este servicio se encarga de comunicarse con el back-end para obtener y editar los datos del cliente.

El código utiliza el patrón de diseño "Reactive Forms" de Angular para validar y enviar el formulario. También utiliza el servicio `ToastrService` para mostrar mensajes de éxito o error al usuario.

El código está bien documentado con comentarios, lo que hace que sea fácil de entender y mantener.