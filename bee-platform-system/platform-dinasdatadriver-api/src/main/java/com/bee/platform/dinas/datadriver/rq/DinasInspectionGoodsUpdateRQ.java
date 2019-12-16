package com.bee.platform.dinas.datadriver.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName DinasInspectionGoodsSaveRQ
 * @Description 功能描述
 * @Date 2019/8/13 17:08
 **/
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("砂石验货磅单修改请求参数")
public class DinasInspectionGoodsUpdateRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @ApiModelProperty("id")
    @NotNull(message = "id不能为空")
    private Integer id;

    /**
     * 采购合同id
     */
    @ApiModelProperty("采购合同id")
    @NotNull(message = "id不能为空")
    private Integer purchaseOrderId;
    /**
     * 销售合同id
     */
    @ApiModelProperty("销售合同id")
    @NotNull(message = "id不能为空")
    private Integer saleOrderId;
    /**
     * 产品id
     */
    @ApiModelProperty("产品id")
    @NotNull(message = "id不能为空")
    private Integer productId;
    /**
     * 产品规格id
     */
    @ApiModelProperty("产品规格id")
    @NotNull(message = "id不能为空")
    private Integer productSpecId;
    /**
     * 数量
     */
    @ApiModelProperty("数量")
    @NotNull(message = "数量不能为空")
    @Min(value = 0,message = "数量大于等于0")
    private BigDecimal num;

    @ApiModelProperty("附件地址")
    private String url;
    /**
     * 附件地址
     */
//    @ApiModelProperty("附件地址")
//    @Size(max=3,message = "附件最多三张")
//    private List<String> urlList;
    /**
     * 验货日期
     */
    @ApiModelProperty("验货日期")
    @NotNull(message = "验货日期不能为空")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date inspectionDate;


}
