package com.bee.platform.user.dto;

import java.io.Serializable;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.Accessors;

/**
 * @author cheng.ke
 * @version 1.0.0
 * @ClassName ArticlesTypeDTO
 * @Description 公告类型返回信息
 * @Date 2019/5/7 10:20
 **/

@Data
@ToString
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("公告类型返回信息")
public class ArticlesTypeDTO implements Serializable{
    /**
	 * 
	 */
	private static final long serialVersionUID = -1959087093603418702L;

	@ApiModelProperty("公告类型id")
    private Integer id;

    @ApiModelProperty("公告类型名称")
    private String name;

}
