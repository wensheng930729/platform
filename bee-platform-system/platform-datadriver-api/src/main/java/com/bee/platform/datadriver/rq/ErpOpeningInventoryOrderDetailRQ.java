package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.math.BigDecimal;

/**
 * @ClassName ErpOpeningInventoryOrderDetailDTO
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/5/30$ 17:09$
 * @version 1.0.0
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("期初库存明细请求信息")
public class ErpOpeningInventoryOrderDetailRQ implements Serializable {

    private static final long serialVersionUID = 6285995015865023553L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("期初库存表id")
    @NotNull(message = "主表id不能为空")
    private Integer openingInventoryOrderId;

    /**
     * 公司id
     */
    @ApiModelProperty("公司id")
    @NotNull(message = "公司id不能为空")
    private Integer companyId;
    /**
     * 公司名称
     */
    @ApiModelProperty("公司名称")
//    @NotEmpty(message = "公司名称不能为空")
    private String companyName;

    @ApiModelProperty("产品id")
    @NotNull(message = "产品id不能为空")
    private Integer productId;

    @ApiModelProperty("产品名称")
//    @NotEmpty(message = "产品名称不能为空")
    private String productName;

    /**
     * 仓库id
     */
    @ApiModelProperty("仓库id")
    @NotNull(message = "仓库id不能为空")
    private Integer repositoryId;

    @ApiModelProperty("仓库名称")
//    @NotEmpty(message = "仓库名称不能为空")
    private String storeHouseName;

    @ApiModelProperty("计量单位")
    @NotEmpty(message = "计量单位不能为空")
    private String unitOfMeasurement;
    /**
     * 化验单id
     */
    @ApiModelProperty("化验单id")
    private Integer testReportId;

    @ApiModelProperty("化验单")
    private String testOrder;

    @ApiModelProperty("期初数量")
    @NotNull(message = "期初数量不能为空")
    @Min(value = 0,message = "期初数量不能小于0")
    private BigDecimal quantity;

}
