package com.bee.platform.user.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

/**
 * @author cheng.ke
 * @version 1.0.0
 * @ClassName IndustryRQ
 * @Description 行业添加信息
 * @Date 2019/4/25 10:30
 **/

@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "添加行业请求参数")
public class IndustryRQ implements Serializable{

    /**
	 * 
	 */
	private static final long serialVersionUID = 2698130439862549136L;
	@ApiModelProperty("id")
    private Integer id;
    /**
     * 父级行业关系
     */
    @ApiModelProperty("父级id")
    @NotNull(message = "父级id不能为空")
    private Integer pid;
    /**
     * 行业名称
     */
    @ApiModelProperty("行业名称")
    @NotEmpty(message = "行业名称不能为空")
    private String industry;
    /**
     * 子属级别关系
     */
    @ApiModelProperty("分类级别")
    @NotNull(message = "所属分类级别不能为空")
    private Integer level;
}
