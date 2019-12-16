package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import org.hibernate.validator.constraints.Range;

import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.List;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName ErpInventoryFlowSearchRQ
 * @Description 功能描述
 * @Date 2019/5/31 14:42
 **/


@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("库存流水帐请求参数")
public class ErpInventoryFlowSearchRQ implements Serializable {
    private static final long serialVersionUID = 1L;

    @ApiModelProperty("业务类型 0 采购入库 1 领料出库 2 成品入库 3 销售出库")
    @NotNull(message = "业务类型不能为空")
    @Range(min = 0,max = 3)
    private Integer type;


    @ApiModelProperty("公司id")
    private Integer companyId;

    @ApiModelProperty("仓库id")
    private String storeHouseName;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("源单据号")
    private String sourceOrder;

    @ApiModelProperty("开始时间")
    private String startTime;

    @ApiModelProperty("结束时间")
    private String endTime;

    private List<Integer> list;
}
