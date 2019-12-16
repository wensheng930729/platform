package com.bee.platform.datadriver.rq;

import java.io.Serializable;

import javax.validation.constraints.NotNull;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("仓库档案删除信息")
public class ErpRepositoryDeleteRQ implements Serializable{

	private static final long serialVersionUID = 1L;

	@ApiModelProperty("仓库档案id")
	@NotNull
    private Integer Id;
	
	@ApiModelProperty("是否删除")
	private Integer deleted;
}
