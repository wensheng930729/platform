package com.bee.platform.datadriver.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.Date;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName ErpGetOneWarehousingOrderRQ
 * @Description 功能描述
 * @Date 2019/6/18 14:01
 **/

@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("化验单管理处获取一条成品入库单号")
public class ErpGetOneWarehousingOrderRQ implements Serializable {


    private static final long serialVersionUID = 1L;

    @ApiModelProperty("公司id")
    @NotNull(message = "公司id不能为空")
    private Integer companyId;

    @ApiModelProperty("产品id")
    @NotNull(message = "产品id不能为空")
    private Integer productId;

    @ApiModelProperty("炉号")
    @NotNull(message = "炉号不能为空")
    private Integer furnaceId;

    @ApiModelProperty("班次")
    @NotNull(message = "班次不能为空")
    private String classes;

    @ApiModelProperty("入库日期")
    @NotNull(message = "入库日期不能为空")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date warehousingTime;

}
