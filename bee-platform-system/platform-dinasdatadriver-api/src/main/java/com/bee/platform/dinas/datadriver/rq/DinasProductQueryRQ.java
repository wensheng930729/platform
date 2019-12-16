package com.bee.platform.dinas.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @author liang.li
 * @ClassName DinasProductList2DTO
 * @Description 砂石产品add的RQ
 * @Date 2019-8-13
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "砂石产品搜索list的RQ")
public class DinasProductQueryRQ implements Serializable {

    private static final long serialVersionUID = -2056179173858396800L;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("是否启用-0禁用1启用")
    private Integer status;

}
