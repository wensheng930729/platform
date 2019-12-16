package com.bee.platform.datadriver.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import org.hibernate.validator.constraints.Length;

import java.io.Serializable;
import java.util.Date;

/**
 * @ClassName WorkPromoteSaveRQ
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/6/24$ 10:54$
 * @version 1.0.0
 */

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("crm工作推进保存请求信息")
public class WorkPromoteSaveRQ implements Serializable {

    private static final long serialVersionUID = -1649147367858052541L;

    @ApiModelProperty("id")
    private Integer id;
    /**
     * 商机信息id
     */
    private Integer commercialId;

    @ApiModelProperty("拜访人")
    private String visitingName;

    @ApiModelProperty("拜访日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date visitingTime;

    @ApiModelProperty("拜访说明")
    @Length(max = 255,message = "字数超过限制")
    private String visitingExplain;
}
