package com.bee.platform.datadriver.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * @ClassName ErpOpeningInventoryOrderSearchListDTO
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/5/30$ 10:37$
 * @version 1.0.0
 */

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("期初库存主表列表返回信息")
@JsonInclude
public class ErpOpeningInventoryOrderSearchListDTO implements Serializable {

    private static final long serialVersionUID = -2354737682756233770L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("公司id")
    private Integer companyId;

    @ApiModelProperty("公司名称")
    private String companyName;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("仓库名称")
    private String storeHouseName;

    @ApiModelProperty("计量单位")
    private String unit;

    @ApiModelProperty("化验单")
    private String testOrder;

    @ApiModelProperty("期初数量")
    private BigDecimal quantity;

    @ApiModelProperty("数据状态(0已保存，1已确认)")
    private Integer state;

}
