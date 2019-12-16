package com.bee.platform.costcontroller.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * erp成本小工具-成本配置
 * </p>
 *
 * @author liliang123
 * @since 2019-06-24
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "erp成本配置列表DTO")
public class ErpCostAllocationQueryDTO implements Serializable {

    private static final long serialVersionUID = 1L;


    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("公司id")
    private Integer companyId;

    @ApiModelProperty("公司id")
    private String companyName;

    @ApiModelProperty("成本配置名称")
    private String name;

    @ApiModelProperty("创建人id")
    private Integer creatorId;

    @ApiModelProperty("创建人姓名")
    private String creator;

    @ApiModelProperty("创建时间")
    private Date createTime;

    @ApiModelProperty("状态")
    private Integer status;

}
