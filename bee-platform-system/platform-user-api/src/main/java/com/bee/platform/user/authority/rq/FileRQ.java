package com.bee.platform.user.authority.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

/**
 * @author liang.li
 * @ClassName FileRQ
 * @Description 文件name和url的rq
 * @Date 2019-5-23
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel(value = "文件name和url的rq")
public class FileRQ {

    @ApiModelProperty("附件名称")
    @NotEmpty(message = "附件名称不能为空")
    private String fileName;

    @ApiModelProperty("附件url")
    @NotEmpty(message = "附件url不能为空")
    private String fileUrl;

    @ApiModelProperty("附件类型（0营业执照 1营业许可证 2企业认证授权书 3logo）")
    @NotNull(message = "类型不能为空")
    private Integer type;
}
