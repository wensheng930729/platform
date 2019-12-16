package com.bee.platform.dinas.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.List;

/**
 * @author liang.li
 * @ClassName DinasProductUpdateRQ
 * @Description 砂石产品update的RQ
 * @Date 2019-8-13
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "砂石产品update的RQ")
public class DinasProductUpdateRQ implements Serializable {

    private static final long serialVersionUID = -2056179173858396800L;

    @ApiModelProperty("产品id")
    @NotNull(message = "产品id不能为空")
    private Integer id;

    @ApiModelProperty("产品名称")
    @NotEmpty(message = "产品名称不能为空")
    private String productName;

    @ApiModelProperty("产品单位")
    @NotEmpty(message = "产品单位不能为空")
    private String unit;

    @ApiModelProperty("是否启用-0禁用1启用")
    @NotNull(message = "是否启用不能为空")
    private Integer status;

    @ApiModelProperty("产品批次")
    private List<String> specList;


}
