package com.bee.platform.user.dto;

import java.io.Serializable;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("待办信息封装类")
public class TodoListDTO implements Serializable {
	
	private static final long serialVersionUID = -1443559995654177592L;

	@ApiModelProperty(value = "待办数量（工作台待办）")
	private int countTodoWorkbenchTask;

	@ApiModelProperty(value = "待办数量（工单待办）")
	private int countTodoWorkOrders;

}
