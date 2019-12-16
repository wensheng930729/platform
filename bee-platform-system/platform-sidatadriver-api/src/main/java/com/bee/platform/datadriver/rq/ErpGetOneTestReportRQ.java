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
 * @ClassName ErpGetOneTestReportRQ
 * @Description 功能描述
 * @Date 2019/6/18 14:01
 **/

@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("根据入库单信息查询化验单")
public class ErpGetOneTestReportRQ implements Serializable {

    private static final long serialVersionUID = -5983130844278668724L;

    @ApiModelProperty("公司id")
    @NotNull(message = "公司id不能为空")
    private Integer company;

    @ApiModelProperty("产品id")
    @NotNull(message = "产品id不能为空")
    private Integer product;

    @ApiModelProperty("炉号")
    @NotNull(message = "炉号不能为空")
    private String boilerId;

    @ApiModelProperty("班次")
    @NotNull(message = "班次不能为空")
    private String shiftsId;

    @ApiModelProperty("生产日期")
    @NotNull(message = "生产日期不能为空")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date productDate;

}
