package com.bee.platform.user.dto;

import java.io.Serializable;
import java.util.List;

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
@ApiModel("列表信息")
public class EnterprisesCheckDTO implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ApiModelProperty("数量")
	private Integer count;
	
	@ApiModelProperty("列表信息")
	private List<EnterprisesCheckApplyDTO> lists;

}
